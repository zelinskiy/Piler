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
      :>     ("id"       :> Get '[JSON] (Entity Device)
         :<|> "status"   :> Get '[JSON] DeviceStatus
         :<|> "refill"
           :> Capture "mid" (Key Medicament)
           :> Capture "quantity" Int
           :> Get '[JSON] Int
         :<|> "dispence"
           :> Capture "mid" (Key Medicament)
           :> Get '[JSON] DeviceStatus)
    

server :: ConnectionPool -> Entity User -> Server API
server p me =
       (myDevice
  :<|> myDeviceStatus
  :<|> refill
  :<|> dispence)
  where
    myDevice = do 
      mbDevice <- exPool p $
        selectFirst [DeviceUserId ==. entityKey me] []
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
    refill mid quant = do
      did <- entityKey <$> myDevice
      exPool p $ do
        mbStorage :: Maybe (Entity DeviceStorage) <- selectFirst
              [ DeviceStorageMedicamentId ==. mid
              , DeviceStorageDeviceId ==. did ] []
        sid <- case mbStorage of
          Just s -> return (entityKey s)
          Nothing -> insert $ DeviceStorage 0 mid did
        deviceStorageQuantity
          <$> updateGet sid [DeviceStorageQuantity +=. quant]
                     
    dispence = undefined

