module HomePage ( module Export
                , Event(..)
                , foldp
                , view
                ) where

import Prelude

import Data.Int (toNumber)
import Data.String(joinWith)
import Data.Foldable(for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Monad.Aff (delay)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Control.Bind((=<<))
import Pux (EffModel, noEffects, onlyEffects, mapEffects)
import Pux.Form(field, form, (.|))
import Pux.DOM.Events (DOMEvent, onClick, targetValue)
import Pux.DOM.HTML.Attributes (style)
import Pux.DOM.HTML (HTML, mapEvent)
import Text.Smolder.HTML (span, button, br, p, hr, h3, h4, input)
import Text.Smolder.HTML.Attributes (type', value)
import Text.Smolder.Markup ((!), (#!), text)
import CSS (color, red, green)
import CSS.Geometry(marginRight, width)
import CSS.Size(em)
import Data.HTTP.Method (Method (POST, GET))
import Data.Argonaut (encodeJson)

import Data.Lens.Setter((.~))
import Data.Lens.Getter((^.))
import Data.Lens as L

import Config(serverRoot)

import Utils.Request(request, request')
import Utils.Other(eitherConsoleEvent, mapi)

import Types.User
import Types.Medicament

import Types.Device
import Types.DeviceStorage
import Types.DeviceStatus

import Types.TreatmentPlan
import Types.TreatmentPlanRow
import Types.FullTreatmentPlan

import HomePage.Effects(Effects) as Export
import HomePage.Effects(Effects)

import HomePage.State as Export
import HomePage.State

import HomePage.Components.TreatmentComponent as Treatment
import HomePage.Components.NavigationComponent as Navigation
import HomePage.Components.DeviceComponent as DeviceComponent
import HomePage.Components.ShoppingComponent as Shopping


data Event
  = Init
  | Tick (Array Event)
  | HideDebug

  | NavigationEvent Navigation.Event
  | DeviceEvent DeviceComponent.Event
  | TreatmentEvent Treatment.Event
  | ShoppingEvent Shopping.Event

  | MedicamentsRequest
  | MedicamentsResponse (Array Medicament)
  | ReplaceNewMedicament Medicament
  | AddMedicamentRequest


initEvents :: Array Event
initEvents = [Tick tickEvents
             , NavigationEvent Navigation.MyselfRequest]

tickEvents :: Array Event
tickEvents = [ DeviceEvent DeviceComponent.DeviceStatusRequest
             , DeviceEvent DeviceComponent.AliveRequest
             , MedicamentsRequest
             , TreatmentEvent Treatment.TreatmentsRequest
             , ShoppingEvent Shopping.ShoppingListsRequest]

foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)

-- Utility

foldp Init st =
  { state: init st.jwt # active .~ true
  , effects: mapi mapping initEvents }
  where
    mapping i e =
      delay (Milliseconds (toNumber i * 100.0)) $> Just e

foldp (Tick _) st@{ active: false } = noEffects st

foldp (Tick events) st@{ active: true } =
  onlyEffects st 
    $ map (pure <<< Just) events
    <> [delay (Milliseconds 1000.0) $> Just (Tick events)]

foldp HideDebug st = noEffects $ st # error .~ ""

-- Components

foldp (NavigationEvent ev) st = 
  Navigation.foldp ev st
  # mapEffects NavigationEvent

foldp (DeviceEvent ev) st = 
  DeviceComponent.foldp ev st
  # mapEffects DeviceEvent

foldp (TreatmentEvent ev) st = 
  Treatment.foldp ev st
  # mapEffects TreatmentEvent

foldp (ShoppingEvent ev) st = 
  Shopping.foldp ev st
  # mapEffects ShoppingEvent

  
-- Medicaments

foldp MedicamentsRequest st = onlyEffects st
  [ let path = serverRoot <> "private/medicament/all/"
    in eitherConsoleEvent MedicamentsResponse
       =<< request st.jwt GET path Nothing ]

foldp (MedicamentsResponse meds) st =
  noEffects $ st # medicaments .~ meds

foldp (ReplaceNewMedicament m) st =
  noEffects $ st # newMedicament .~ m

foldp AddMedicamentRequest st =
  { state: st { newMedicament = defaultMedicament }
  , effects:
    [ let path = serverRoot <> "private/medicament/add/"
          d = Just $ encodeJson st.newMedicament
      in request' st.jwt POST path d $> Nothing
    ]
  }




----------------------------------------------
--             VIEW                         --
----------------------------------------------

  
view :: State -> HTML Event
view st = do
  mapEvent NavigationEvent $ Navigation.view st
  hr
  when (st.error /= "") do
    p
      ! style (color red)
      $ text st.error
    button
      #! onClick (const HideDebug)
      $ text "Close"
  when (st ^. me ^. status == "Admin") renderNewMedicament
  mapEvent DeviceEvent $ DeviceComponent.view st
  mapEvent TreatmentEvent $ Treatment.view st
  mapEvent ShoppingEvent $ Shopping.view st

  where
    renderNewMedicament = do
      p $ text "Add new Medicament"
      let btn = button
                ! type' "button" 
                #! onClick (const AddMedicamentRequest)
                $ text "Add"
          fields =
            field name 
            <> (description
                <<< L.lens (fromMaybe "None") (const Just)
               .| flip (*>) btn)
      form st.newMedicament fields ReplaceNewMedicament


