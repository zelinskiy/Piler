module Types.Login where

import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

import Data.Newtype (class Newtype)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Symbol (SProxy(..))

newtype Login = Login
                { email :: String
                , pass :: String
                , ip :: String} 

defaultLogin :: Login
defaultLogin = Login { email: "user1@mail.com"
                     , pass: "pass"
                     , ip: "127.0.0.1:8070" }

derive instance eqLogin :: Eq Login

derive instance genericLogin :: Generic Login

instance decodeJsonLogin :: DecodeJson Login where
  decodeJson = A.decodeJson

instance encodeJsonLogin :: EncodeJson Login where
  encodeJson = A.encodeJson


derive instance newtypeLogin :: Newtype Login _

email :: Lens' Login String
email = _Newtype <<< prop (SProxy :: SProxy "email")

pass :: Lens' Login String
pass = _Newtype <<< prop (SProxy :: SProxy "pass")

ip :: Lens' Login String
ip = _Newtype <<< prop (SProxy :: SProxy "ip")
