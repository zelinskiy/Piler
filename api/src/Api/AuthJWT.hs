{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Api.AuthJWT
  ( Login(..)
  , Private
  , PublicAPI
  , publicServer)
where

import Control.Monad.Trans (liftIO)
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
          (Headers '[ Header "Set-Cookie" SetCookie
                    , Header "Set-Cookie" SetCookie]
           NoContent)

publicServer :: CookieSettings
             -> JWTSettings
             -> PublicServer PublicAPI
publicServer cs jwts (Login e p) = do
  mUsr <- db2 $
    selectFirst [ UserEmail    ==. e
                , UserPassword ==. hash p ] []
  mApplyCookies <- case mUsr of
     Nothing ->
       throwError $ err401
       { errBody = "Can't find user" }
     Just usr ->
       liftIO $ acceptLogin cs jwts usr
  case mApplyCookies of
    Nothing ->
      throwError $ err401
      { errBody = "Can't apply cookie" }
    Just applyCookies ->
      return $ applyCookies NoContent
       

