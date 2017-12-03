{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Auth
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
import Data.List
import qualified Data.Map as Map

import Model
import Utils

type instance AuthServerData (AuthProtect "cookie-auth") = Entity User

type Private = AuthProtect "cookie-auth" 

throw401 m = throwError (err401 { errBody = m })

authContext :: ConnectionPool
            -> AuthHandler Request (Entity User)
authContext pool = mkAuthHandler $ \req ->
  let h = requestHeaders req in
  case (lookup "email" h, lookup "password" h) of
    (Nothing, Nothing) -> 
      throw401 "Missing password & email"
    (Nothing, Just p) ->
      throw401 "Missing email"
    (Just e, Nothing) ->
      throw401 "Missing password"
    (Just e, Just p) -> do
      mbUser <- exPool pool $
        selectFirst [ UserEmail    ==. unpack e
                            , UserPassword ==. unpack p] []
                                        
      case mbUser of
        Just u -> return u
        Nothing ->
          throwError (err403 { errBody = "Can't find user" })




