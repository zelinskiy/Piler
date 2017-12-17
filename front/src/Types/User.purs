module Types.User where

import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

import Data.Newtype (class Newtype)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Symbol (SProxy(..))

-- also returns pass (hash)
newtype User = User
               { email :: String
               , status :: String } 

defaultUser :: User
defaultUser = User { email: "username@mail.com"
                   , status: "Unknown" }

derive instance eqUser :: Eq User

derive instance genericUser :: Generic User

instance decodeJsonUser :: DecodeJson User where
  decodeJson = A.decodeJson

instance encodeJsonUser :: EncodeJson User where
  encodeJson = A.encodeJson

derive instance newtypeUser :: Newtype User _

email :: Lens' User String
email = _Newtype <<< prop (SProxy :: SProxy "email")

status :: Lens' User String
status = _Newtype <<< prop (SProxy :: SProxy "status")
