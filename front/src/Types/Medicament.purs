module Types.Medicament where

import Data.Maybe(Maybe)
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)

import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

newtype Medicament = Medicament
                { id :: Int
                , name :: String
                , diameter :: Int
                , height :: Int
                , description :: Maybe String } 

derive instance genericMedicament :: Generic Medicament

instance decodeJsonMedicament :: DecodeJson Medicament where
  decodeJson = A.decodeJson

instance encodeJsonMedicament :: EncodeJson Medicament where
  encodeJson = A.encodeJson


