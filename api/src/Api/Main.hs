module Api.Main(API, server) where

import Database.Persist.Sqlite
import Servant
import Servant.Auth.Server

import qualified Api.Medicament
import qualified Api.Treatment
import qualified Api.Device
import qualified Api.User
import qualified Api.AuthJWT
import qualified Api.Auth
import qualified Api.Admin
import qualified Api.Shopping

import Model
import Utils


type API = 
       "public"   :> PublicApi
  :<|> "private"  :> Api.AuthJWT.Private :> PrivateApi
  :<|> "private2" :> Api.Auth.Private    :> PrivateApi

type PublicApi =
       "greeting" :> Get '[JSON] String
  :<|> "user"     :> Api.User.PublicAPI
  :<|> "jwt"      :> Api.AuthJWT.PublicAPI

type PrivateApi =
       "medicament" :> Api.Medicament.API
  :<|> "treatment"  :> Api.Treatment.API
  :<|> "device"     :> Api.Device.API
  :<|> "user"       :> Api.User.API
  :<|> "admin"      :> Api.Admin.API
  :<|> "shopping"   :> Api.Shopping.API
  
server :: ConnectionPool
       -> CookieSettings
       -> JWTSettings
       -> Server API
server p c jwt = publicServer p c jwt
                 :<|> privateServer p
                 :<|> privateServer p . Authenticated

publicServer :: ConnectionPool
             -> CookieSettings
             -> JWTSettings
             -> Server PublicApi
publicServer p c jwt = enter (publicToNormalH p) $
       return "Greetings!"
  :<|> Api.User.publicServer
  :<|> Api.AuthJWT.publicServer c jwt

privateServer :: ConnectionPool
              -> AuthResult (Entity User)
              -> Server PrivateApi
privateServer p (Authenticated u) =
  enter (privateToPublicH u `ver` publicToNormalH p) $
       Api.Medicament.server
  :<|> Api.Treatment.server
  :<|> Api.Device.server
  :<|> Api.User.server
  :<|> Api.Admin.server
  :<|> Api.Shopping.server
privateServer _ _ = throwAll err401
