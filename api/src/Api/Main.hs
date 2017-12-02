{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Main(app, startApp) where

import Network.Wai.Handler.Warp
import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Control.Monad.Logger (runStdoutLoggingT)
import Data.String.Conversions
import Servant
import Data.Aeson
import Servant.Server.Experimental.Auth
import Network.Wai
import GHC.Generics
import Data.ByteString (ByteString)
import Data.Map (Map, fromList)
import qualified Data.Map as Map

import Model
import qualified Api.Medicament
import qualified Api.Treatment
import qualified Api.Device
import qualified Api.User
import Api.Auth

sqlitePath = "sqlite.db"

type PrivateApi =
       "medicament" :> Api.Medicament.API
  :<|> "treatment"  :> Api.Treatment.API
  :<|> "device"     :> Api.Device.API
  :<|> "user"       :> Api.User.API
  
type PublicApi =
       "greeting" :> Get '[JSON] String
  :<|> "user"     :> Api.User.PublicAPI

type API =
       "public"  :> PublicApi
  :<|> "private" :> Private   :> PrivateApi
  
server :: ConnectionPool -> Server API
server p =
           (return "Greetings!"
       :<|> Api.User.publicServer p)
  :<|> \u ->
            Api.Medicament.server p u
       :<|> Api.Treatment.server  p u
       :<|> Api.Device.server     p u
       :<|> Api.User.server       p u
       
              
getConnectionPool :: IO ConnectionPool
getConnectionPool = do
  pool <- runStdoutLoggingT $
    createSqlitePool sqlitePath 5
  runSqlPool (runMigration migrateAll) pool
  return pool

app :: IO Application
app = do
  pool <- getConnectionPool
  return $ serveWithContext api
    (authContext pool) (server pool)

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = run 8080 =<< app
