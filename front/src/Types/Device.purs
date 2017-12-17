module Types.Device where

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

import Data.Newtype (class Newtype)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Symbol (SProxy(..))
import Data.Function((<<<))

--Device

-- also returns userId
newtype Device = Device { ip :: String }

derive instance genericDevice :: Generic Device

instance decodeJsonDevice :: DecodeJson Device where
  decodeJson = A.decodeJson

instance encodeJsonDevice :: EncodeJson Device where
  encodeJson = A.encodeJson

derive instance newtypeDevice :: Newtype Device _

ip :: Lens' Device String
ip = _Newtype <<< prop (SProxy :: SProxy "ip")
