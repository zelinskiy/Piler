{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module MainApi
    ( startApp
    , app
    ) where

import Network.Wai.Handler.Warp
import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)
import Data.String.Conversions
import Servant

import Model
import qualified MedicamentApi

sqlitePath = "sqlite.db"

type API = "medicaments" :> MedicamentApi.API

server :: ConnectionPool -> Server API
server pool =
  MedicamentApi.server pool

app :: IO Application
app = do
  pool <- getConnectionPool
  return $ serve api $ server pool

getConnectionPool :: IO ConnectionPool
getConnectionPool = do
  pool <- runStderrLoggingT $ do
    createSqlitePool sqlitePath 5

  runSqlPool (runMigration migrateAll) pool
  return pool

api :: Proxy API
api = Proxy

startApp :: IO ()
startApp = run 8080 =<< app
