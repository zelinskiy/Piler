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

data Configuration
  = Configuration
  { serverUrl :: String
  , clientHost :: String
  , clientPort :: Int
  , devicePort :: FilePath
  , deviceStorage :: TVar Int
  } deriving (Eq, Generic)



type API = "dispence" :> Get '[JSON] Int
         :<|> "alarm" :> Get '[JSON] ()

server :: Configuration -> Server API
server c = dispence :<|> alarm
  where
    dispence = liftIO $ do
      let svar = deviceStorage c
      s <- atomically $ do
        modifyTVar svar (+1)
        readTVar svar  
      print s
      return s
    alarm = liftIO $ putStrLn "ALARM!"

app :: Configuration -> Application
app c = serve (Proxy :: Proxy API) (server c)

cliLoop :: Configuration -> IO ()
cliLoop configs = do
  cmds <- words <$> getLine
  case cmds of
    ["refill", medname, quantity] -> do
      let route = "/refill/" <> cs medname
                  <> "/"     <> cs quantity 
      get route >>= print
      
    ["cmd", cmd] -> putStrLn "not implemented"
    
    ["status"] -> putStrLn "not implemented"
    
    ["e"] -> putStrLn "exit"
    
    _ -> print (unwords cmds)
    
  cliLoop configs  
  where
    server  = serverUrl configs
    client  = clientHost configs
              <> show (clientPort configs)
    port    = devicePort configs
    storage = deviceStorage configs
    root    = "http://"
              <> server
              <> "/private/device/my"
    get r   = do      
      let req = setRequestMethod "GET"
            -- $ setRequestHost (cs $ clientHost configs)
            -- $ setRequestPort (clientPort configs)
            $ parseRequest_ (root <> r)
      print req
      httpLBS req


startApp :: IO ()
startApp = do  
  let tty = "dev/tty"
  [cp] <- getArgs
  
  storage <- atomically $ newTVar 0
  let configs = Configuration
        { serverUrl     = "localhost:8080"
        , clientHost    = "127.0.0.1"
        , clientPort    = read cp
        , devicePort    = tty
        , deviceStorage = storage}
  forkIO $ do
    putStrLn "Started CLI"
    cliLoop configs
  run (clientPort configs) (app configs)

