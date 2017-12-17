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

import Data.Lens.Setter((.~))
import Data.Lens.Getter((^.))

import Config(serverRoot)

import Utils.Request(request)
import Utils.Other(eitherConsoleEvent, constUnit, mapi)

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

data Event
  = Init
  | Tick (Array Event)
  | HideDebug

  | NavigationEvent Navigation.Event
  | DeviceEvent DeviceComponent.Event
  | TreatmentEvent Treatment.Event

  | MedicamentsRequest
  | MedicamentsResponse (Array Medicament)

  
  

initEvents :: Array Event
initEvents = [Tick tickEvents
             , NavigationEvent Navigation.MyselfRequest]

tickEvents :: Array Event
tickEvents = [ DeviceEvent DeviceComponent.DeviceStatusRequest
             , MedicamentsRequest
             , TreatmentEvent Treatment.TreatmentsRequest ]

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

  
-- Medicaments

foldp MedicamentsRequest st = onlyEffects st
  [ let path = serverRoot <> "private/medicament/all/"
    in eitherConsoleEvent MedicamentsResponse
       =<< request st.jwt GET path Nothing ]

foldp (MedicamentsResponse meds) st =
  noEffects $ st # medicaments .~ meds


----------------------------------------------
--             VIEW                         --
----------------------------------------------

  
view :: State -> HTML Event
view s = do
  mapEvent NavigationEvent $ Navigation.view s
  hr
  when (s.error /= "") do
    p
      ! style (color red)
      $ text s.error
    button
      #! onClick (const HideDebug)
      $ text "Close"
  mapEvent DeviceEvent $ DeviceComponent.view s
  mapEvent TreatmentEvent $ Treatment.view s


