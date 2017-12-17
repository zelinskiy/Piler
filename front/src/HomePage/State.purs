module HomePage.State where

import Prelude
import Data.Function((#))
import Data.Maybe(Maybe(Nothing), fromMaybe)
import Data.Array(head, filter)

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))

import Utils.Request(JWT)
import Types.DeviceStatus
import Types.DeviceStorage
import Types.FullTreatmentPlan
import Types.Medicament
import Types.User

type State = { deviceStatus :: Maybe DeviceStatus
             , treatment :: Array FullTreatmentPlan
             , medicaments :: Array Medicament
             , me :: User
             , jwt :: JWT
             , prompting :: Boolean
             , error :: String
             , active :: Boolean }

init :: JWT -> State
init jwt = { deviceStatus: Nothing
           , treatment: []
           , medicaments : []
           , me: defaultUser
           , jwt: jwt
           , prompting: false
           , error: ""
           , active: false}

-- Lenses

deviceStatus :: Lens' State (Maybe DeviceStatus)
deviceStatus = prop (SProxy :: SProxy "deviceStatus")

treatment :: Lens' State (Array FullTreatmentPlan)
treatment = prop (SProxy :: SProxy "treatment")

medicaments :: Lens' State (Array Medicament)
medicaments = prop (SProxy :: SProxy "medicaments")

me :: Lens' State User
me = prop (SProxy :: SProxy "me")

jwt :: Lens' State JWT
jwt = prop (SProxy :: SProxy "jwt")

prompting :: Lens' State Boolean
prompting = prop (SProxy :: SProxy "prompting")

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
