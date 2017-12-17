module Types.TreatmentPlan where

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

import Data.Newtype (class Newtype)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Symbol (SProxy(..))
import Data.Function((<<<))

-- also returns deviceId
newtype TreatmentPlan = TreatmentPlan { id :: Int } 

derive instance genericTreatmentPlan :: Generic TreatmentPlan

instance decodeJsonTreatmentPlan :: DecodeJson TreatmentPlan where
  decodeJson = A.decodeJson

instance encodeJsonTreatmentPlan :: EncodeJson TreatmentPlan where
  encodeJson = A.encodeJson

derive instance newtypeTreatmentPlan :: Newtype TreatmentPlan _

id :: Lens' TreatmentPlan Int
id = _Newtype <<< prop (SProxy :: SProxy "id")

