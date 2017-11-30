{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AuthApi
    ( authContext
    , Private
    ) where

import Network.Wai.Handler.Warp
import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)
import Data.String.Conversions
import Servant
import Data.Aeson
import Servant.Server.Experimental.Auth
import Network.Wai
import Data.ByteString (ByteString)
import Data.ByteString.Char8(unpack)
import Data.Map (Map, fromList)
import qualified Data.Map as Map

import Model

type instance AuthServerData (AuthProtect "cookie-auth") = User

type Private = AuthProtect "cookie-auth" 

database :: [User]
database =
  [ User "user1@mail.com" "pass"
  , User "user2@mail.com" "pass"
  , User "user3@mail.com" "pass" ]

authContext :: ConnectionPool -> Context (AuthHandler Request User ': '[])
authContext pool = mkAuthHandler handler :. EmptyContext
  where
    handler req =
      let h = requestHeaders req in
      case (lookup "email" h, lookup "password" h) of
        (Nothing, Nothing) ->
          throwError (err401 { errBody = "Missing email and password" })
        (Nothing, Just p) ->
          throwError (err401 { errBody = "Missing email" })
        (Just e, Nothing) ->
          throwError (err401 { errBody = "Missing password" })  
        (Just e, Just p) ->
          let u = User (unpack e) (unpack p) in
          if u `elem` database
          then return u
          else throwError (err403 { errBody = "Can't find user" })




