module HomePage ( State(..)
                , Event(..)
                , Effects(..)
                , foldp
                , view
                , init
                ) where

import Prelude

import Data.Int (toNumber)
import Data.Array(head, filter)
import Data.String(joinWith)
import Data.Foldable(for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Monad.Aff (delay)
import Data.Time.Duration (Milliseconds(Milliseconds))

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

import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Data.Lens.Setter((.~))
import Data.Lens.Getter((^.))

import Config(serverRoot)

import Utils.Request(JWT, request)
import Utils.Other(eitherEvents, mapi)

import Types.User
import Types.Medicament

import Types.Device
import Types.DeviceStorage
import Types.DeviceStatus

import Types.TreatmentPlan
import Types.TreatmentPlanRow
import Types.FullTreatmentPlan




data Event
  = Init
  | Tick (Array Event)
  | ShowDebug String
  | HideDebug
    
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

  | MyselfRequest
  | MyselfResponse User
  

type Effects fx =
  ( ajax :: AJAX
  , console :: CONSOLE
  , dom :: DOM | fx)

type State = { deviceStatus :: Maybe DeviceStatus
             , treatment :: Array FullTreatmentPlan
             , medicaments :: Array Medicament
             , me :: User
             , jwt :: JWT
             , prompting :: Boolean
             , error :: String }

init :: JWT -> State
init jwt = { deviceStatus: Nothing
           , treatment: []
           , medicaments : []
           , me: defaultUser
           , jwt: jwt
           , prompting: false
           , error: "" }

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

----------------------------------------------
--             UPDATE                       --
----------------------------------------------

initEvents :: Array Event
initEvents = [Tick tickEvents, MyselfRequest]

tickEvents :: Array Event
tickEvents = [ DeviceStatusRequest
             , MedicamentsRequest
             , TreatmentsRequest ]

foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)

-- Utility events

foldp Init st =
  { state: init st.jwt
  , effects: mapi mapping initEvents }
  where
    mapping i e =
      delay (Milliseconds (toNumber i * 100.0)) $> Just e

foldp (Tick events) st =
  onlyEffects st 
    $ map (pure <<< Just) events
    <> [delay (Milliseconds 1000.0) $> Just (Tick events)]

foldp (ShowDebug m) st = noEffects $ st # error .~ m
foldp HideDebug st = noEffects $ st # error .~ ""

-- Navigation events

foldp SignOutRequest _ = noEffects (init "")
  
foldp SubscribeRequest st =
  onlyEffects st [ pure $ Just AskSecretCode ]
  
foldp SeizeTheMeansRequest st = onlyEffects st
  [ let path = serverRoot <> "private/admin/seize/the/means"
    in eitherEvents ShowDebug ShowDebug
       <$> request st.jwt GET path Nothing ]

foldp AskSecretCode st = noEffects $ st {prompting = true}
foldp DontSaySecretCode st = noEffects $ st {prompting = false}
foldp (SaySecretCode ev) st =
  { state: st # prompting .~ false 
  , effects:
    [ let path = serverRoot
               <> "private/user/upgrade/SubscribeSilver/"
               <> targetValue ev
      in eitherEvents ShowDebug ShowDebug
         <$> request st.jwt GET path Nothing ]
  }
  
-- Device

foldp DeviceStatusRequest st = onlyEffects st
  [ let path = serverRoot <> "private/device/my/status/"
    in eitherEvents ShowDebug DeviceStatusResponse
       <$> request st.jwt GET path Nothing ]

foldp (DeviceStatusResponse ds) st =
  noEffects $ st # deviceStatus .~ Just ds 

-- Treatment

foldp TreatmentsRequest st = onlyEffects st
  [ let path = serverRoot <> "private/treatment/my/full/"
    in eitherEvents ShowDebug TreatmentsResponse
       <$> request st.jwt GET path Nothing ]

foldp (TreatmentsResponse tps) st =
  noEffects $ st # treatment .~ tps

-- Medicaments

foldp MedicamentsRequest st = onlyEffects st
  [ let path = serverRoot <> "private/medicament/all/"
    in eitherEvents ShowDebug MedicamentsResponse
       <$> request st.jwt GET path Nothing ]

foldp (MedicamentsResponse meds) st =
  noEffects $ st # medicaments .~ meds

-- Myself

foldp MyselfRequest st = onlyEffects st
  [ let path = serverRoot <> "private/user/me/"
    in eitherEvents ShowDebug MyselfResponse
       <$> request st.jwt GET path Nothing ]

foldp (MyselfResponse me) st =
  noEffects $ st { me = me }

----------------------------------------------
--             VIEW                         --
----------------------------------------------

  
view :: State -> HTML Event
view s@{ error: e } = do
  navigationView s
  hr
  when (e /= "") do
    p
      ! style (color red)
      $ text e
    button
      #! onClick (const HideDebug)
      $ text "Close"
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
  
navigationView st = do
  span
    ! style do
        color green
        marginRight (10.0 # em)
    $ text ("Logged as " <> st ^. me ^. email)
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
deviceView st@{ deviceStatus: Just (DeviceStatus s) }  = do
  h3 $ text "Your device status:"
  p $ text $ "ip: " <> ip
  h4 $ text "Storage:"
  for_ s.storage renderStorage
  where
    ip = (\(Device d) -> d.ip) s.device
    renderStorage (DeviceStorage storage) = do
      renderQuantity storage.quantity
      span $ text " of "
      renderMedicament storage.medicamentId
      br
    renderQuantity q =
      if q <= 0
      then s ! style (color red)
      else s
      where s = span $ text (show q)
    renderMedicament m =
      getMedicament st m
      # map (\(Medicament m) -> m.name)
      # fromMaybe "Unknown"
      # span <<< text

deviceView { deviceStatus: Nothing } = do
  h3 $ text "Device not loaded"
  button
    #! onClick (const DeviceStatusRequest)
    $ text "reload"
  
-- TODO: Rewrite this with lenses
treatmentView :: State -> HTML Event
treatmentView { treatment: [] } =
  h3 $ text "No treatment plans"
treatmentView st@{ treatment: tps } = do
  h3 $ text "Device:"
  for_ tps renderPlan
  where    
    renderPlan (FullTreatmentPlan p) = do
      h4 $ text $ "Treatment plan #"
        <> show (getPlanId p.treatmentPlan)
      for_ p.treatmentPlanRows renderPlanRow
      
    getPlanId (TreatmentPlan p) = p.id
    
    renderPlanRow (TreatmentPlanRow r) =
      p $ text $        
        "Take " <> show r.quantity <> " of "
        <> fromMaybe "Unknown" (getMedicamentName r.medicamentId)
        <> " at " <> r.at
        <> " (Stored: " <> show (howMuchStored r.medicamentId) <> ")"
         
        
    getMedicamentName =
      getMedicament st
      >>> map (\(Medicament m) -> m.name)
          
    howMuchStored =
      getMedicamentStored st
      >>> map (\(DeviceStorage ds) -> ds.quantity)
      >>> fromMaybe 0

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
