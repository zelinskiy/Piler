{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Services.TickTack (run) where

import Database.Persist.Sql hiding (get)
import Control.Concurrent
import Control.Monad.Logger
import Control.Monad.Trans
import Data.Time.Clock
import qualified Data.Text as T
import Data.Monoid
import Control.Monad
import Network.HTTP.Simple
import qualified Data.ByteString.Lazy as BL

import Data.String.Conversions

import Model


-- Doing some scheduling
run :: (MonadIO m, MonadLogger m)
    => ConnectionPool -> m ()
run pool =  do
  logInfoN "Started TickTack Service."
  go pool

go :: (MonadIO m, MonadLogger m)
   => ConnectionPool -> m ()
go pool = do
  let delay = 30
      exPool p = liftIO . flip runSqlPersistMPool p  
      sleep = liftIO $ threadDelay (fromInteger delay*10^6)
      
  now <- liftIO getCurrentTime
  pending <- exPool pool $ selectList
    [TreatmentPlanRowAt <=. now] []
  
  when (not (null pending)) $ do
    logInfoN $ "TickTack sent "
      <> T.pack (show (length pending))
      <> " notification(s)"
    forM_ pending $ \r -> do     
      device <- exPool pool $ updateStorage r
      dispenceDevice device r      
      return ()    
  sleep
  go pool
  
  
-- send dispence command to device
dispenceDevice :: MonadIO m
               => Entity Device
               -> Entity TreatmentPlanRow
               -> m (Response BL.ByteString)
dispenceDevice device r =          
  httpLBS
  $ setRequestMethod "GET"
  $ parseRequest_
  $ "http://" <> cs ip
    <> "/dispence"
    <> "/" <> show m
    <> "/" <> show n
  where
    ip = deviceIp $ entityVal device
    [PersistInt64 m] = keyToValues
       $ treatmentPlanRowMedicamentId (entityVal r)
    n = treatmentPlanRowQuantity (entityVal r)

-- update storages
updateStorage r = do
  let rv = entityVal r
      
  Just plan <- getEntity (treatmentPlanRowTreatmentPlanId rv)
      
  Just device <- getEntity (treatmentPlanDeviceId (entityVal plan))
    
  mbStorage <- selectFirst
    [DeviceStorageDeviceId ==. entityKey device] []

  let storage =
        case mbStorage of
          Nothing -> error "Notify: refill please"
          Just s -> s
    
  updateWhere
    [ DeviceStorageId ==. entityKey storage
    , DeviceStorageMedicamentId
      ==. treatmentPlanRowMedicamentId rv]
    [ DeviceStorageQuantity -=.
      treatmentPlanRowQuantity rv]
  deleteWhere
    [TreatmentPlanRowId ==. entityKey r]

  return device
