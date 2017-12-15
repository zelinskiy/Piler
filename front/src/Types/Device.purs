module Types.Device where

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)

import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

--Device

-- also returns userId
newtype Device = Device { ip :: String }

derive instance genericDevice :: Generic Device

instance decodeJsonDevice :: DecodeJson Device where
  decodeJson = A.decodeJson

instance encodeJsonDevice :: EncodeJson Device where
  encodeJson = A.encodeJson

-- DeviceStorage

-- also returns deviceId
newtype DeviceStorage = DeviceStorage
                        { quantity :: Int
                        , medicamentId :: Int } 

derive instance genericDeviceStorage :: Generic DeviceStorage

instance decodeJsonDeviceStorage :: DecodeJson DeviceStorage where
  decodeJson = A.decodeJson

instance encodeJsonDeviceStorage :: EncodeJson DeviceStorage where
  encodeJson = A.encodeJson

-- DeviceStatus

newtype DeviceStatus = DeviceStatus
                       { device :: Device
                       , storage :: Array DeviceStorage } 

derive instance genericDeviceStatus :: Generic DeviceStatus

instance decodeJsonDeviceStatus :: DecodeJson DeviceStatus where
  decodeJson = A.decodeJson

instance encodeJsonDeviceStatus :: EncodeJson DeviceStatus where
  encodeJson = A.encodeJson


