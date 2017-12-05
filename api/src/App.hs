{-# LANGUAGE OverloadedStrings #-}

module App (app, startApp) where

import Network.Wai.Handler.Warp
import Database.Persist.Sqlite
import Servant
import Servant.Auth.Server
import Control.Monad.IO.Class
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Time
import Control.Concurrent

import qualified Model
import qualified Api.AuthJWT
import qualified Api.Auth
import qualified Api.Main
import qualified TickTack

sqlitePath = "sqlite.db"
port = 8080

getConnectionPool :: IO ConnectionPool
getConnectionPool = do
  pool <- runStdoutLoggingT $
    createSqlitePool sqlitePath 5
  runSqlPool (runMigration Model.migrateAll) pool
  return pool

app :: IO Application
app = do
  pool <- getConnectionPool
  k <- generateKey
  let cs = defaultCookieSettings
         { cookieMaxAge = Just $ secondsToDiffTime 3600 }
      jwt = defaultJWTSettings k
      jwtContext = cs :. jwt :. EmptyContext
      ctx = Api.Auth.authContext pool
            :. cs :. jwt :. EmptyContext
      api = Proxy :: Proxy Api.Main.API

  forkIO $ TickTack.run pool
  
  return $ serveWithContext api ctx (Api.Main.server pool cs jwt)

startApp :: IO ()
startApp = run port =<< app
