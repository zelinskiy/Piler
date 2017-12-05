{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Api.Device
    ( API
    , server
    ) where

import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Servant
import Data.Either
import Data.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource.Internal
import Data.Aeson
import GHC.Generics

import Model
import JsonModel(DeviceStatus(..))
import Utils

type API =
         "my"
      :> ("id" :> Get '[JSON] (Entity Device)
         :<|> "status" :> Get '[JSON] DeviceStatus)
    

server :: ConnectionPool -> Entity User -> Server API
server p me =
       (myDevice
  :<|> myDeviceStatus)
  where
    myDevice = do 
      mbDevice <- exPool p $
        selectFirst [DeviceId ==. userDeviceId (entityVal me)] []
      case mbDevice of
        Just d -> return d
        Nothing -> throwError err403
    myDeviceStatus = do
      d <- myDevice
      s <- exPool p $
        selectList [DeviceStorageDeviceId ==. entityKey d] []
      return DeviceStatus
        { device = entityVal d
        , storage = map entityVal s }
