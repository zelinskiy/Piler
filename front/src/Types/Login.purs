module Types.Login where

import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)

import Utils.Other

newtype Login = Login
                { email :: String
                , pass :: String } 

defaultLogin :: Login
defaultLogin = Login { email: "user1@mail.com"
                     , pass: "pass" }


derive instance genericLogin :: Generic Login

instance decodeJsonLogin :: DecodeJson Login where
  decodeJson = genericDecodeNewtypeJson

instance encodeJsonLogin :: EncodeJson Login where
  encodeJson = genericEncodeNewtypeJson


