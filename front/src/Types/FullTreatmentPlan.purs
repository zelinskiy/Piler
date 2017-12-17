module Types.FullTreatmentPlan where

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

import Data.Newtype (class Newtype)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Symbol (SProxy(..))
import Data.Function((<<<))

import Types.TreatmentPlan(TreatmentPlan)
import Types.TreatmentPlanRow(TreatmentPlanRow)

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
  
derive instance newtypeFullTreatmentPlan :: Newtype FullTreatmentPlan _

treatmentPlan :: Lens' FullTreatmentPlan TreatmentPlan
treatmentPlan = _Newtype <<< prop (SProxy :: SProxy "treatmentPlan")

treatmentPlanRows :: Lens' FullTreatmentPlan (Array TreatmentPlanRow)
treatmentPlanRows =
  _Newtype <<< prop (SProxy :: SProxy "treatmentPlanRows")
