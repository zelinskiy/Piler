{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module App (startApp, app) where

import Data.Aeson
import Network.Wai
import Network.Wai.Handler.Warp
import GHC.Generics
import Servant
import System.Environment
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import Data.String.Conversions
import Data.Monoid
import Network.HTTP.Simple hiding (Proxy)
import Control.Monad.IO.Class
import System.Hardware.Serialport
import System.Exit
import System.Directory
import System.IO
import qualified Data.ByteString.Lazy as LB
import Data.Aeson
import Data.ByteString.Internal (ByteString, c2w)
import qualified Data.ByteString.Internal as BI
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString as BS

testMedicamentId = 1

type JWT = ByteString

data Login = Login
  { email :: String
  , pass :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Login
instance FromJSON Login

data Configuration
  = Configuration
  { serverUrl :: String
  , clientHost :: String
  , clientPort :: Int
  , devicePort :: SerialPort
  , token :: JWT
  , userEmail :: String
  , userPass :: String }

type API =
  "dispence"
    :> Capture "mid" Int
    :> Capture "n" Int
    :> Get '[JSON] ()
  :<|> "alarm"
    :> Get '[JSON] ()

server :: Configuration -> Server API
server c = dispence :<|> alarm
  where
    h = devicePort c
    jwt = token c
    root = serverUrl c <> "/private/device/my"
    dispence mid n
      | mid /= testMedicamentId =
        throwError $ err403
        { errBody = "Multiple meds yet not suported" }
      | n < 0 =
        throwError $ err403
        { errBody = "Can`t dispence negative" }
    dispence _ n = liftIO $ do
      send h (BS.replicate n (c2w 'D'))
      get jwt (root <> "/status")
      >>= C8.putStrLn . getResponseBody
    alarm = liftIO $ do
      send h "S"
      putStrLn "ALARM!"
    

cliLoop :: Configuration -> IO ()
cliLoop c = do
  cmds <- words <$> getLine
  case cmds of
    ["refill", medname, quantity] ->
      let route = root
            <> "/refill/" <> cs medname
            <> "/"        <> cs quantity 
      in get jwt route
      >>= C8.putStrLn . getResponseBody

    ["pullout", medname, quantity] ->
      let route = root <> "/pullout"
            <> "/" <> cs medname
            <> "/" <> cs quantity 
      in get jwt route
      >>= C8.putStrLn . getResponseBody
      
    ["cmd", cmd] ->
      send h (cs cmd) >> return ()
    
    ["status"] ->
      get jwt (root <> "/status")
      >>= C8.putStrLn . getResponseBody

    ["id"] ->
      get jwt (root <> "/id")
      >>= C8.putStrLn . getResponseBody

    ["rebind", "ip", ip] ->
      putStrLn "Not implemented"
    
    ["q"] -> do
      closeSerial h
      putStrLn "goodbye!"
      exitWith ExitSuccess

    ["help"] -> putStrLn help
    ["h"] -> putStrLn help

    ["help", cmd] -> putStrLn $ helpCmd cmd
    
    _ -> putStrLn (unwords cmds)
    
  cliLoop c
  where
    jwt = token c
    h = devicePort c
    root = serverUrl c <> "/private/device/my"
    help = unlines
      [ "refill medname n         - refill storage"
      , "pullout medname n        - empty storage"
      , "cmd [{F,B,D,S,L,N,W,M}]  - raw command"
      , "status                   - get device status"
      , "id                       - get device id"
      , "rebind ip IP             - rebind device ip"
      , "q                        - exit"
      , "{help|h}                 - show this help"
      , "help [{F,B,D,S,L,N,W,M}] - cmds meaning" ]
    helpCmd "F" = "Pull caret [F]orward"
    helpCmd "B" = "Pull caret [B]ackward"
    helpCmd "D" = "[D]ispence pill = FB"
    helpCmd "S" = "[S]ound alarm"
    helpCmd "L" = "[L]ight alarm"
    helpCmd "N" = "Do [N]othing"
    helpCmd "W" = "Un[M]ute (W = flipped M)"
    helpCmd "M" = "[M]ute sound"


getConfig :: IO Configuration
getConfig = withFile "config.txt" ReadMode $ \h -> do
  [cp, tty] <- getArgs
  [e, p] <- lines <$> hGetContents h
  h <- openSerial tty defaultSerialSettings
      { commSpeed = CS9600 }
  return Configuration
    { serverUrl  = "http://localhost:8080"
    , clientHost = "localhost"
    , clientPort = read cp
    , devicePort = h
    , token      = "Put JWT here!"
    , userEmail  = e
    , userPass   = p }

getJWT :: Configuration -> IO JWT
getJWT c = do
  let route = serverUrl c <> "/public/jwt/login"
      login = Login (userEmail c) (userPass c)
      chop = drop 1
           . takeWhile (/= ';')
           . dropWhile (/= '=')
      findJWT (h:hs)
        | "JWT-Cookie" `isPrefixOf` h = chop h
        | otherwise = findJWT hs
  cs <$> findJWT <$> map cs
     <$> getResponseHeader "Set-Cookie"
     <$> postJson "" route login

get jwt route
  = httpLBS
  $ setRequestMethod "GET"
  $ addRequestHeader "Authorization" ("Bearer " <> jwt)
  $ parseRequest_ route

postJson jwt route body
  = httpLBS
  $ setRequestMethod "POST"
  $ addRequestHeader "Authorization" ("Bearer " <> jwt)
  $ addRequestHeader "Content-Type" "application/json"
  $ setRequestBodyJSON body
  $ parseRequest_ route

app :: Configuration -> Application
app c = serve (Proxy :: Proxy API) (server c)

startApp :: IO ()
startApp = do
  c0 <- getConfig
  jwt <- getJWT c0

  let c = c0 { token = jwt }
      
  forkIO $ do
    putStrLn "Started Server"
    run (clientPort c) (app c)
  
  putStrLn "Started CLI"
  cliLoop c
