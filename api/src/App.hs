{-# LANGUAGE OverloadedStrings #-}

module App (app, startApp) where

import Network.Wai.Handler.Warp
import Database.Persist.Sqlite
import Servant
import Servant.Auth.Server
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Time
import Control.Concurrent
import Data.Text(Text)
import Network.Wai.Middleware.Cors

import qualified Model
import qualified Api.Auth
import qualified Api.Main
import qualified Services.TickTack as TickTack

sqlitePath :: Text
sqlitePath = "sqlite.db"

port :: Int
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
      ctx = Api.Auth.authContext pool
            :. cs :. jwt :. EmptyContext
      api = Proxy :: Proxy Api.Main.API

  forkIO $ runStdoutLoggingT $ TickTack.run pool
  return $ serveWithContext api ctx (Api.Main.server pool cs jwt)


startApp :: IO ()
startApp = run port . myCors =<< app
  where
    myCors = cors (const $ Just policy)
    policy = simpleCorsResourcePolicy
      { corsRequestHeaders = [ "Accept"
                             , "Accept-Language"
                             , "Content-Language"
                             , "Access-Control-Allow-Origin"
                             , "Content-Type" ]
      , corsMethods = [ "GET"
                      , "HEAD"
                      , "POST"
                      , "PUT"
                      , "PATCH"]
      }
