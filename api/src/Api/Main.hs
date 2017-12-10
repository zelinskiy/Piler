{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Main(API, server) where

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
import Servant.Auth.Server
import Data.Time

import Model
import qualified Api.Medicament
import qualified Api.Treatment
import qualified Api.Device
import qualified Api.User
import qualified Api.AuthJWT
import qualified Api.Auth
import qualified Api.Admin
import qualified Api.Shopping

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
publicServer p c jwt =
       return "Greetings!"
  :<|> Api.User.publicServer p
  :<|> Api.AuthJWT.publicServer p c jwt

privateServer :: ConnectionPool
              -> AuthResult (Entity User)
              -> Server PrivateApi
privateServer p (Authenticated u) =
       Api.Medicament.server p u
  :<|> Api.Treatment.server  p u
  :<|> Api.Device.server     p u
  :<|> Api.User.server       p u
  :<|> Api.Admin.server      p u
  :<|> Api.Shopping.server   p u
privateServer _ _ = throwAll err401


