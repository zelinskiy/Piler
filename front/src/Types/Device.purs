module Types.Device where

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)

import Utils.Other(genericDecodeNewtypeJson, genericEncodeNewtypeJson)

--Device

-- also returns userId
newtype Device = Device { ip :: String }

derive instance genericDevice :: Generic Device

instance decodeJsonDevice :: DecodeJson Device where
  decodeJson = genericDecodeNewtypeJson

instance encodeJsonDevice :: EncodeJson Device where
  encodeJson = genericEncodeNewtypeJson

-- DeviceStorage

-- also returns deviceId
newtype DeviceStorage = DeviceStorage
                        { quantity :: Int
                        , medicamentId :: Int } 

derive instance genericDeviceStorage :: Generic DeviceStorage

instance decodeJsonDeviceStorage :: DecodeJson DeviceStorage where
  decodeJson = genericDecodeNewtypeJson

instance encodeJsonDeviceStorage :: EncodeJson DeviceStorage where
  encodeJson = genericEncodeNewtypeJson

-- DeviceStatus

newtype DeviceStatus = DeviceStatus
                       { device :: Device
                       , storage :: Array DeviceStorage } 

derive instance genericDeviceStatus :: Generic DeviceStatus

instance decodeJsonDeviceStatus :: DecodeJson DeviceStatus where
  decodeJson = genericDecodeNewtypeJson

instance encodeJsonDeviceStatus :: EncodeJson DeviceStatus where
  encodeJson = genericEncodeNewtypeJson


