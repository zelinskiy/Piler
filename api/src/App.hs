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
--import qualified  Data.Text.IO as T
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as BS
--import Servant.Docs
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

showLayout :: IO ()
showLayout = do
  pool <- getConnectionPool
  k <- generateKey
  let cs = defaultCookieSettings
         { cookieMaxAge = Just $ secondsToDiffTime 3600 }
      jwt = defaultJWTSettings k
      ctx = Api.Auth.authContext pool
            :. cs :. jwt :. EmptyContext
      api = Proxy :: Proxy Api.Main.API
  BS.putStrLn $ encodeUtf8 $ layoutWithContext api ctx


startApp :: IO ()
startApp = run port . myCors =<< app
  where
    myCors = cors (const $ Just policy)
    policy = CorsResourcePolicy
      { corsOrigins = Nothing
      , corsMethods = [ "GET"
                      , "HEAD"
                      , "POST"
                      , "DELETE"
                      , "PUT"
                      , "PATCH"]
      , corsRequestHeaders = [ "Accept"
                             , "Accept-Language"
                             , "Content-Language"
                             , "Access-Control-Allow-Origin"
                             , "Content-Type"
                             , "Set-Cookie"
                             , "Authorization"]
      , corsExposedHeaders = Just [ "Access-Control-Allow-Origin"
                                  , "Content-Type"
                                  , "Set-Cookie" ]
      , corsMaxAge = Nothing
      , corsVaryOrigin = False
      , corsRequireOrigin = False
      , corsIgnoreFailures = False }

    

