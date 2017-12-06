{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Server  where

import Servant
import Data.String.Conversions
import Network.HTTP.Simple hiding (Proxy)
import Control.Monad.IO.Class
import System.Hardware.Serialport
import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString as BS

import Types (Configuration(..))
import Utils

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
      -- | mid /= testMedicamentId =
      --   throwError $ err403
      --   { errBody = "Multiple meds yet not suported" }
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
    
