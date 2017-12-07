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
import Network.HTTP.Simple
import Network.HTTP.Types
import Data.String.Conversions

import Model
import JsonModel(DeviceStatus(..))
import Utils

--TODO: many devices for 1 user

type API =
         "my"
      :>     ("all"      :> Get '[JSON] (Entity Device)
         :<|> "id"       :> Get '[JSON] (Key Device)
         :<|> "status"   :> Get '[JSON] DeviceStatus
         :<|> "stored"
           :> Capture "mid" (Key Medicament)
           :> Get '[JSON] Int
         :<|> "refill"
           :> Capture "mid" (Key Medicament)
           :> Capture "quantity" Int
           :> Get '[JSON] Int
         :<|> "pullout"
           :> Capture "mid" (Key Medicament)
           :> Capture "quantity" Int
           :> Get '[JSON] Int
         :<|> "dispence"
           :> Capture "mid" (Key Medicament)
           :> Capture "quantity" Int
           :> Get '[JSON] DeviceStatus)
             
    

server :: ConnectionPool -> Entity User -> Server API
server p me =
       (myDevice
  :<|> myDeviceId
  :<|> myDeviceStatus
  :<|> storedOf
  :<|> refill
  :<|> pullout
  :<|> dispence)
  where
    myDevice = do 
      mbDevice <- exPool p $
        selectFirst [DeviceUserId ==. entityKey me] []
      case mbDevice of
        Just d -> return d
        Nothing -> throwError err403
    myDeviceId = entityKey <$> myDevice
    myDeviceStatus = do
      d <- myDevice
      s <- exPool p $
        selectList [DeviceStorageDeviceId ==. entityKey d] []
      return DeviceStatus
        { device = entityVal d
        , storage = map entityVal s }
        
    storedOf mid = do
      did <- entityKey <$> myDevice
      mbStorage <- exPool p $ selectFirst
              [ DeviceStorageMedicamentId ==. mid
              , DeviceStorageDeviceId ==. did ] []
      return $ case mbStorage of
        Nothing -> 0
        Just s  -> deviceStorageQuantity (entityVal s)
         
    updateStorage mid n = do
      did <- entityKey <$> myDevice
      exPool p $ do
        mbStorage <- selectFirst
              [ DeviceStorageMedicamentId ==. mid
              , DeviceStorageDeviceId ==. did ] []
        sid <- case mbStorage of
          Just s -> return (entityKey s)
          Nothing -> insert $ DeviceStorage 0 mid did
        deviceStorageQuantity
          <$> updateGet sid [DeviceStorageQuantity +=. n]
          
    refill _ n | n < 0 = throwError $ err403
      { errBody = "can't be negative" }
    refill mid n = updateStorage mid n
    
    pullout _ n | n < 0 = throwError $ err403
      { errBody = "can't be negative" }
    pullout mid n = updateStorage mid (negate n)
    
    dispence mid n = do
      ip <- deviceIp <$> entityVal <$> myDevice
      let [PersistInt64 m] = keyToValues mid
          route = "http://" <> cs ip
                  <> "/dispence"
                  <> "/" <> show m
                  <> "/" <> show n
      pullout mid n
      httpLBS $ setRequestMethod "GET"
        $ parseRequest_ route
      myDeviceStatus

