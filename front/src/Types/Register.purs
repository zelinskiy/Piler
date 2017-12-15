module Types.Register where

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)

import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

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


