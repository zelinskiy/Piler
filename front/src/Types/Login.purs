module Types.Login where

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)

import Data.Argonaut.Encode.Generic.Rep (gEncodeJson)
import Data.Argonaut.Decode.Generic.Rep (gDecodeJson)

newtype Login = Login
                { email :: String
                , pass :: String } 

defaultLogin :: Login
defaultLogin = Login { email: "user1@mail.com"
                     , pass: "pass" }
{-
derive instance genericLogin :: Generic Login

instance decodeJsonLogin :: DecodeJson Login where
  decodeJson = gDecodeJson


instance encodeJsonLogin :: EncodeJson Login where
  encodeJson = gEncodeJson
-}
{-
instance decodeJsonLogin :: DecodeJson Login where
  decodeJson json = do
    o <- decodeJson json
    email <- o .? "email"
    pass <- o .? "pass"
    pure $ Login { email, pass }

instance encodeJsonLogin :: EncodeJson Login where
  encodeJson (Login { email: e, pass: p })
     = "email" := e
    ~> "pass" := p
    ~> jsonEmptyObject
-}
