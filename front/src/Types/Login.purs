module Types.Login where

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)

import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

newtype Login = Login
                { email :: String
                , pass :: String } 

defaultLogin :: Login
defaultLogin = Login { email: "user1@mail.com"
                     , pass: "pass" }


derive instance genericLogin :: Generic Login

instance decodeJsonLogin :: DecodeJson Login where
  decodeJson = A.decodeJson

instance encodeJsonLogin :: EncodeJson Login where
  encodeJson = A.encodeJson


