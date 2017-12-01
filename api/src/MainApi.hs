{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MainApi(startApp) where

import Network.Wai.Handler.Warp
import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)
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
import qualified MedicamentApi
import qualified TreatmentApi
import AuthApi

sqlitePath = "sqlite.db"

type PrivateApi =
       "medicament" :> MedicamentApi.API
  :<|> "treatment"  :> TreatmentApi.API
  
type PublicApi = "greeting" :> Get '[JSON] String

type API =
       "public"  :> PublicApi
  :<|> "private" :> Private   :> PrivateApi
  
server :: ConnectionPool -> Server API
server pool =
       return "Greetings!"
  :<|> \user ->
            MedicamentApi.server pool user
       :<|> TreatmentApi.server pool user
              
getConnectionPool :: IO ConnectionPool
getConnectionPool = do
  pool <- runStderrLoggingT $
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
