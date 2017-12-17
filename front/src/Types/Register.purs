module Types.Register where

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

import Data.Newtype (class Newtype)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Symbol (SProxy(..))
import Data.Function((<<<))

newtype Register = Register
                   { email :: String
                   , pass  :: String
                   , ip    :: String } 

defaultRegister :: Register
defaultRegister = Register { email: "user1@mail.com"
                           , pass: "pass"
                           , ip: "127.0.0.1:8070" }

derive instance genericRegister :: Generic Register

instance decodeJsonLogin :: DecodeJson Register where
  decodeJson = A.decodeJson

instance encodeJsonLogin :: EncodeJson Register where
  encodeJson = A.encodeJson


derive instance newtypeRegister :: Newtype Register _

email :: Lens' Register String
email = _Newtype <<< prop (SProxy :: SProxy "email")

pass :: Lens' Register String
pass = _Newtype <<< prop (SProxy :: SProxy "pass")

ip :: Lens' Register String
ip = _Newtype <<< prop (SProxy :: SProxy "ip")
