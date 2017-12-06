{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
 
module JsonModel where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Model

data Login = Login
  { email :: String
  , pass :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Login
instance FromJSON Login

data RegisterData = RegisterData
  { email :: String
  , pass  :: String
  , ip    :: String
  } deriving (Eq, Show, Generic)

instance ToJSON RegisterData
instance FromJSON RegisterData

data DeviceStatus = DeviceStatus
                  { device :: Device
                  , storage :: [DeviceStorage] }
                  deriving (Eq, Show, Generic)

instance ToJSON DeviceStatus
instance FromJSON DeviceStatus

data FullTreatmentPlan
  = FullTreatmentPlan
    { treatmentPlan :: TreatmentPlan
    , treatmentPlanRows :: [TreatmentPlanRow]
    } deriving (Eq, Show, Generic)

instance ToJSON FullTreatmentPlan
instance FromJSON FullTreatmentPlan
