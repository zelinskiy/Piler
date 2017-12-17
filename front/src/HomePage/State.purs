module HomePage.State where

import Prelude
import Data.Tuple
import Data.Function((#))
import Data.Maybe(Maybe(Just, Nothing), fromMaybe)
import Data.Array(head, filter)

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

import Utils.Request(JWT)
import Types.DeviceStatus
import Types.DeviceStorage
import Types.TreatmentPlanRow
import Types.FullTreatmentPlan
import Types.Medicament
import Types.User

type State = { deviceStatus :: Maybe DeviceStatus
             , treatment :: Array FullTreatmentPlan
             , newTreatmentRow :: TreatmentPlanRow
             , medicaments :: Array Medicament
             , newListName :: String
             , me :: User
             , jwt :: JWT
             , prompt :: Tuple Boolean String
             , error :: String
             , active :: Boolean }

init :: JWT -> State
init jwt = { deviceStatus: Nothing
           , treatment: []
           , newTreatmentRow: defaultTreatmentPlanRow
           , medicaments : []
           , newListName: "My Shopping List"
           , me: defaultUser
           , jwt: jwt
           , prompt: Tuple false "Contact admin for secret code please"
           , error: ""
           , active: false}

-- Lenses

deviceStatus :: Lens' State (Maybe DeviceStatus)
deviceStatus = prop (SProxy :: SProxy "deviceStatus")

treatment :: Lens' State (Array FullTreatmentPlan)
treatment = prop (SProxy :: SProxy "treatment")

newTreatmentRow :: Lens' State TreatmentPlanRow
newTreatmentRow = prop (SProxy :: SProxy "newTreatmentRow")

medicaments :: Lens' State (Array Medicament)
medicaments = prop (SProxy :: SProxy "medicaments")

me :: Lens' State User
me = prop (SProxy :: SProxy "me")

jwt :: Lens' State JWT
jwt = prop (SProxy :: SProxy "jwt")

prompt :: Lens' State (Tuple Boolean String)
prompt = prop (SProxy :: SProxy "prompt")

error :: Lens' State String
error = prop (SProxy :: SProxy "error")

active :: Lens' State Boolean
active = prop (SProxy :: SProxy "active")

-- Utils

-- TODO: Rewrite this with lenses
getMedicament :: State -> Int -> Maybe Medicament
getMedicament st mid =
  st.medicaments
  # filter (\(Medicament m) -> m.id == mid)
  # head

getMedicamentStored :: State -> Int -> Maybe DeviceStorage
getMedicamentStored st mid =
  st.deviceStatus
  # map (\(DeviceStatus s) -> s.storage)
  # fromMaybe []
  # filter (\(DeviceStorage ds) -> ds.medicamentId == mid)
  # head
