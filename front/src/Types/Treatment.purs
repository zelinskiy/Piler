module Types.Treatment where

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)

import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

-- TreatmentPlan

-- also returns deviceId
newtype TreatmentPlan = TreatmentPlan { id :: Int } 

derive instance genericTreatmentPlan :: Generic TreatmentPlan

instance decodeJsonTreatmentPlan :: DecodeJson TreatmentPlan where
  decodeJson = A.decodeJson

instance encodeJsonTreatmentPlan :: EncodeJson TreatmentPlan where
  encodeJson = A.encodeJson

-- TreatmentPlanRow

newtype TreatmentPlanRow = TreatmentPlanRow
                           { id :: Int
                           , at :: String
                           , quantity :: Int
                           , medicamentId :: Int
                           , treatmentplanId :: Int} 

derive instance genericTreatmentPlanRow :: Generic TreatmentPlanRow

instance decodeJsonTreatmentPlanRow
         :: DecodeJson TreatmentPlanRow where
  decodeJson = A.decodeJson

instance encodeJsonTreatmentPlanRow
         :: EncodeJson TreatmentPlanRow where
  encodeJson = A.encodeJson

-- FullTreatmentPlan

newtype FullTreatmentPlan =
  FullTreatmentPlan
  { treatmentPlan :: TreatmentPlan
  , treatmentPlanRows :: Array TreatmentPlanRow }

derive instance genericFullTreatmentPlan :: Generic FullTreatmentPlan

instance decodeJsonFullTreatmentPlan 
         :: DecodeJson FullTreatmentPlan where
  decodeJson = A.decodeJson

instance encodeJsonFullTreatmentPlan
         :: EncodeJson FullTreatmentPlan where
  encodeJson = A.encodeJson
  
