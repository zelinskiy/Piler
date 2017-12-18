module Types.TreatmentPlanRow where
import Prelude

import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

import Data.Newtype (class Newtype)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Symbol (SProxy(..))
import Data.Function((<<<))

newtype TreatmentPlanRow = TreatmentPlanRow
                           { id :: Int
                           , at :: String
                           , quantity :: Int
                           , medicamentId :: Int
                           , treatmentPlanId :: Int} 

derive instance genericTreatmentPlanRow :: Generic TreatmentPlanRow

instance decodeJsonTreatmentPlanRow
         :: DecodeJson TreatmentPlanRow where
  decodeJson = A.decodeJson

instance encodeJsonTreatmentPlanRow
         :: EncodeJson TreatmentPlanRow where
  encodeJson = A.encodeJson

defaultTreatmentPlanRow :: TreatmentPlanRow
defaultTreatmentPlanRow =
  TreatmentPlanRow { id: -1
                   , at: "2018-01-01T00:00Z"
                   , quantity: 0
                   , medicamentId: -1
                   , treatmentPlanId: -1 }

derive instance newtypeTreatmentPlanRow :: Newtype TreatmentPlanRow _

id :: Lens' TreatmentPlanRow Int
id = _Newtype <<< prop (SProxy :: SProxy "id")

at :: Lens' TreatmentPlanRow String
at = _Newtype <<< prop (SProxy :: SProxy "at")

quantity :: Lens' TreatmentPlanRow Int
quantity = _Newtype <<< prop (SProxy :: SProxy "quantity")

medicamentId :: Lens' TreatmentPlanRow Int
medicamentId = _Newtype <<< prop (SProxy :: SProxy "medicamentId")

treatmentPlanId :: Lens' TreatmentPlanRow Int
treatmentPlanId = _Newtype <<< prop (SProxy :: SProxy "treatmentPlanId")
