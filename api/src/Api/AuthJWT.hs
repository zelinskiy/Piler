{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.AuthJWT (Login(..), Private, PublicAPI, publicServer) where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.Trans (liftIO)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import System.Environment (getArgs)
import Servant
import Servant.Auth.Server
import Servant.Auth.Server.SetCookieOrphan ()
import Database.Persist.Sqlite

import Model
import JsonModel(Login(..))
import Utils

instance ToJWT (Entity User)
instance FromJWT (Entity User)

type Private = Auth '[JWT] (Entity User)

type PublicAPI =
 "login"
     :> ReqBody '[JSON] Login
     :> PostNoContent '[JSON]
          (Headers '[ Header "Set-Cookie" SetCookie  --JWT
                    , Header "Set-Cookie" SetCookie] --XSRF
           NoContent)

publicServer :: ConnectionPool
             -> CookieSettings
             -> JWTSettings
             -> Server PublicAPI
publicServer pool cookieSettings jwtSettings (Login e p) = do
   mUsr <- exPool pool $ selectFirst [ UserEmail    ==. e
                                     , UserPassword ==. p] []
   mApplyCookies <- case mUsr of
     Nothing ->
       throwError $ err401 { errBody = "Can't find user" }
     Just usr ->
       liftIO $ acceptLogin cookieSettings jwtSettings usr
   case mApplyCookies of
     Nothing ->
       throwError $ err401 { errBody = "Can't apply cookie" }
     Just applyCookies ->
       return $ applyCookies NoContent
       

