module Types.DeviceStatus where

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

import Data.Newtype (class Newtype)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Symbol (SProxy(..))
import Data.Function((<<<))

import Types.Device
import Types.DeviceStorage

newtype DeviceStatus = DeviceStatus
                       { device :: Device
                       , storage :: Array DeviceStorage } 

derive instance genericDeviceStatus :: Generic DeviceStatus

instance decodeJsonDeviceStatus :: DecodeJson DeviceStatus where
  decodeJson = A.decodeJson

instance encodeJsonDeviceStatus :: EncodeJson DeviceStatus where
  encodeJson = A.encodeJson

derive instance newtypeDeviceStatus :: Newtype DeviceStatus _

device :: Lens' DeviceStatus Device
device = _Newtype <<< prop (SProxy :: SProxy "device")

storage :: Lens' DeviceStatus (Array DeviceStorage)
storage = _Newtype <<< prop (SProxy :: SProxy "storage")

