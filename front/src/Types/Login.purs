module Types.Login where

import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson, decodeJson, encodeJson, (:=), jsonEmptyObject, (~>), (.?))
import Data.Generic (class Generic)
import Data.Maybe (fromMaybe)
import Data.Either(Either(..), either)
import Data.Array

--from purescript-maps
import Data.StrMap as M

import Data.Argonaut.Encode.Generic (gEncodeJson)
import Data.Argonaut.Decode.Generic (gDecodeJson)
import Data.Argonaut.Core(fromString, toObject, toArray, fromArray, fromObject, jNull, fromNull)

newtype Login = Login
                { email :: String
                , pass :: String } 

defaultLogin :: Login
defaultLogin = Login { email: "user1@mail.com"
                     , pass: "pass" }
{-
derive instance genericLogin :: Generic Login

instance decodeJsonLogin :: DecodeJson Login where
  decodeJson j = gDecodeJson j'
    where
      j' = fromObject
           $ M.insert "tag" (fromString "Types.Login")
           $ M.insert "values" (decodeJson j)
           $ M.empty

-- $ SM.insert "tag" (fromString (reflectSymbol (SProxy :: SProxy name)))

instance encodeJsonLogin :: EncodeJson Login where
  encodeJson l = 
    let d = gEncodeJson l
    in fromMaybe (fromNull jNull) $ do
      o <- toObject d
      vs <- M.lookup "values" o
      vs' <- toArray vs
      head vs'

wat :: String
wat =
  case decodeJson (encodeJson defaultLogin) of
    Left e -> e
    Right (Login l) -> l.email
-}


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

