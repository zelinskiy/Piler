{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Api.User
    ( API
    , PublicAPI
    , server
    , publicServer
    ) where

import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Servant
import Data.Either
import Data.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource.Internal
import GHC.Generics
import Data.Aeson
import qualified Crypto.Hash.SHA256 as SHA256

import Model
import JsonModel(RegisterData(..))
import Utils

type API =
         "me"
      :> Get '[JSON] (Entity User)
    :<|> "unregister"
      :> Get '[JSON] ()
    :<|> "updrade"
      :> Capture "purpose" SecretKeyPurpose
      :> Capture "key" String
      :> Get '[JSON] ()
    
type PublicAPI =
         "register"
      :> ReqBody '[JSON] RegisterData
      :> Post '[JSON] (Key User)

server :: ConnectionPool -> Entity User -> Server API
server p me =
       getMyself
  :<|> unregister
  :<|> upgrade
  where
    getMyself = return me
    unregister = exPool p $ do      
      deleteCascade (entityKey me)
    upgrade SubscribeSilver _
      | userStatus (entityVal me) > Silver
      = throwError $ err403
        { errBody = "Your status already higher" }
    upgrade SubscribeSilver k =  do
      mbKey <- exPool p $ selectFirst
        [ SecretKeyValue ==. k
        , SecretKeyPurpose ==. SubscribeSilver ] []
      case mbKey of
        Nothing -> throwError $ err403
          { errBody = "Key not Found" }
        Just key -> exPool p $ do          
          update (entityKey me) [UserStatus =. Silver]
          delete (entityKey key)
      

publicServer :: ConnectionPool -> Server PublicAPI
publicServer p = register
  where
    register RegisterData
      { email = email
      , pass = pass
      , ip = ip} = exPool p $ do      
      uid <- insert $ User
        { userEmail = email
        , userPassword = hash pass
        , userStatus = Normal }
      did <- insert $ Device ip uid
      return uid
      
