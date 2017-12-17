module Types.DeviceStorage where

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

import Data.Newtype (class Newtype)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Symbol (SProxy(..))
import Data.Function((<<<))

-- also returns deviceId
newtype DeviceStorage = DeviceStorage
                        { quantity :: Int
                        , medicamentId :: Int } 

derive instance genericDeviceStorage :: Generic DeviceStorage

instance decodeJsonDeviceStorage :: DecodeJson DeviceStorage where
  decodeJson = A.decodeJson

instance encodeJsonDeviceStorage :: EncodeJson DeviceStorage where
  encodeJson = A.encodeJson

derive instance newtypeDeviceStorage :: Newtype DeviceStorage _

quantity :: Lens' DeviceStorage Int
quantity = _Newtype <<< prop (SProxy :: SProxy "quantity")

medicamentId :: Lens' DeviceStorage Int
medicamentId = _Newtype <<< prop (SProxy :: SProxy "medicamentId")
