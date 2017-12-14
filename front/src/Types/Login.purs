module Types.Login where

import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, jsonEmptyObject, (~>), (:=), (.?))

newtype Login = Login
                { email :: String
                , pass :: String } 

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
