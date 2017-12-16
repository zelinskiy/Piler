module HomePage ( State(..)
                , Event(..)
                , Effects(..)
                , foldp
                , view
                , init
                ) where

import Prelude

import Data.Array(head, filter)
import Data.String(joinWith)
import Data.Foldable(for_)
import Data.Either(Either(..), either)
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Monad.Aff (attempt, delay)
import Data.Time.Duration (Milliseconds(Milliseconds))

import Data.Argonaut(decodeJson)
import DOM (DOM)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent, onClick, targetValue)
import Pux.DOM.HTML.Attributes (style)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (span, button, br, p, hr, h3, h4, input)
import Text.Smolder.HTML.Attributes (type', value)
import Text.Smolder.Markup ((!), (#!), text)
import CSS (color, red, green)
import CSS.Geometry(marginRight, width)
import CSS.Size(em)
import Data.HTTP.Method (Method (POST, GET))

import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Console (CONSOLE)

import Utils.Request(getJsonAuth, JWT, request)
import Utils.Other(eitherEvents)

import Types.Device
import Types.Medicament
import Types.Treatment

serverRoot :: String
serverRoot = "http://localhost:8080/"

type State = { deviceStatus :: Maybe DeviceStatus
             , treatment :: Array FullTreatmentPlan
             , medicaments :: Array Medicament
             , jwt :: JWT
             , prompting :: Boolean
             , error :: String }

data Event
  = InitEvent
  | ShowDebug String
    
  | SignOutRequest
  | SubscribeRequest
  | SeizeTheMeansRequest
    
  | AskSecretCode
  | SaySecretCode DOMEvent
  | DontSaySecretCode

  | DeviceStatusRequest
  | DeviceStatusResponse DeviceStatus

  | TreatmentsRequest
  | TreatmentsResponse (Array FullTreatmentPlan)

  | MedicamentsRequest
  | MedicamentsResponse (Array Medicament)
  

type Effects fx =
  ( ajax :: AJAX
  , console :: CONSOLE
  , dom :: DOM | fx)

init :: JWT -> State
init jwt = { deviceStatus: Nothing
           , treatment: []
           , medicaments : []
           , jwt: jwt
           , prompting: false
           , error: "" }


foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)

-- Utility events

foldp InitEvent st = onlyEffects st
  [delay (Milliseconds 2000.0) $> Just DeviceStatusRequest]

foldp (ShowDebug m) st =
  { state: st { error = m }
  , effects:
    [ delay (Milliseconds 6000.0) $> Just (ShowDebug "") ]
  }

-- Navigation events

foldp SignOutRequest _ = noEffects (init "")
  
foldp SubscribeRequest st =
  onlyEffects st [ pure $ Just AskSecretCode ]
  
foldp SeizeTheMeansRequest st@{jwt: jwt} = onlyEffects st
  [ let path = serverRoot <> "private/admin/seize/the/means"
    in eitherEvents ShowDebug ShowDebug
       <$> request jwt GET path Nothing ]

foldp AskSecretCode s = noEffects $ s {prompting = true}
foldp DontSaySecretCode s = noEffects $ s {prompting = false}
foldp (SaySecretCode ev) st@{jwt: jwt} =
  { state: st { prompting = false }
  , effects:
    [ let path = serverRoot
               <> "private/user/upgrade/SubscribeSilver/"
               <> targetValue ev
      in eitherEvents ShowDebug ShowDebug
         <$> request jwt GET path Nothing ]
  }
  
-- Device

foldp DeviceStatusRequest s@{ jwt: jwt } = onlyEffects s
  [ let path = serverRoot <> "private/device/my/status/"
    in eitherEvents ShowDebug DeviceStatusResponse
       <$> request jwt GET path Nothing ]

foldp (DeviceStatusResponse ds) s =
  noEffects $ s { deviceStatus = Just ds }

-- Treatment

foldp TreatmentsRequest st@{jwt: jwt} = onlyEffects st
  [ let path = serverRoot <> "private/treatment/my/full/"
    in eitherEvents ShowDebug TreatmentsResponse
       <$> request jwt GET path Nothing ]

foldp (TreatmentsResponse tps) st =
  noEffects $ st { treatment = tps }

-- Medicaments

foldp MedicamentsRequest st@{jwt: jwt} = onlyEffects st
  [ let path = serverRoot <> "private/medicament/all/"
    in eitherEvents ShowDebug MedicamentsResponse
       <$> request jwt GET path Nothing ]

foldp (MedicamentsResponse meds) st =
  noEffects $ st { medicaments = meds }



  
view :: State -> HTML Event
view s@{ error: e } = do
  navigationView s
  hr
  p
    ! style (color red)
    $ text e
  deviceView s
  treatmentView s
  button
    ! type' "button"
    #! onClick (const MedicamentsRequest)
    $ text "Test"
  
navigationView :: State -> HTML Event
navigationView st | st.prompting = do
  input
    ! style (width $ 20.0 # em)
    ! type' "text"
    ! value "Contact admin for secret code please"
  button
    #! onClick SaySecretCode
    $ text "✓" 
  button
    #! onClick (const DontSaySecretCode)
    $ text "✗" 
  
navigationView _ = do
  span
    ! style do
        color green
        marginRight (10.0 # em)
    $ text "You are logged in."
  button
    #! onClick (const SignOutRequest)
    $ text "Log out"
  button
    #! onClick (const SubscribeRequest)
    $ text "Subscribe"
  button
    ! style (color red)
    #! onClick (const SeizeTheMeansRequest)
    $ text "☭"

medicamentsView :: State -> HTML Event
medicamentsView s@{ medicaments: [] } = do
  h3 $ text "Medicaments ale not loaded"
  
medicamentsView s@{ medicaments: meds } = do  
  h3 $ text "Medicaments database: "
  for_ meds renderMed
  where    
    renderMed (Medicament m) =
      p $ text $ joinWith " "
        [ m.name
        , fromMaybe "" m.description ]

deviceView :: State -> HTML Event
deviceView { deviceStatus: Just (DeviceStatus s) }  = do
  h3 $ text "Your device status:"
  p $ text $ "ip: " <> ip
  h4 $ text "Storage:"
  for_ s.storage renderStorage
  where
    ip = (\(Device d) -> d.ip) s.device
    renderStorage (DeviceStorage { quantity: q, medicamentId: m }) =
      p $ text $ show q <> " of " <> show m

deviceView { deviceStatus: Nothing } = do
  h3 $ text "Device not loaded"
  button
    #! onClick (const DeviceStatusRequest)
    $ text "reload"
  

treatmentView :: State -> HTML Event
treatmentView { treatment: [] } =
  h3 $ text "No treatment plans"
treatmentView st@{ treatment: tps } = do
  h3 $ text "Your treatment schedule:"
  for_ tps renderPlan
  where    
    renderPlan (FullTreatmentPlan p) = do
      h4 $ text $ "Treatment plan #"
        <> show (getPlanId p.treatmentPlan)
      for_ p.treatmentPlanRows renderPlanRow
      
    getPlanId (TreatmentPlan p) = p.id
    
    renderPlanRow (TreatmentPlanRow r) =
      p $ text $ joinWith " "
        [ r.at
        , show r.quantity
        , fromMaybe "" $ getMedicamentName r.medicamentId
        , show $ howMuchStored r.medicamentId
        ]

    getMedicamentName =
      getMedicament st
      >>> map (\(Medicament m) -> m.name)
          
    howMuchStored =
      getMedicamentStored st
      >>> map (\(DeviceStorage ds) -> ds.quantity)


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
