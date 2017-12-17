module HomePage.Components.DeviceComponent
       (Event(..), foldp , view ) where

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
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (span, button, br, p, hr, h3, h4, input)
import Text.Smolder.HTML.Attributes (type', value)
import Text.Smolder.Markup ((!), (#!), text)
import CSS (color, red, green)
import CSS.Geometry(marginRight, width)
import CSS.Size(em)
import Data.HTTP.Method (Method (POST, GET))
import Data.Lens.Setter((.~))

import Config(serverRoot)
import Utils.Request(request)
import Utils.Other(eitherConsoleEvent)
import Types.Medicament
import Types.Device
import Types.DeviceStatus
import Types.DeviceStorage(DeviceStorage(..))
import Types.TreatmentPlan
import Types.TreatmentPlanRow
import Types.FullTreatmentPlan(FullTreatmentPlan(..))

import HomePage.State
import HomePage.Effects(Effects)

data Event
  = DeviceStatusRequest
  | DeviceStatusResponse DeviceStatus

foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)
foldp DeviceStatusRequest st = onlyEffects st
  [ let path = serverRoot <> "private/device/my/status/"
    in eitherConsoleEvent DeviceStatusResponse
       =<< request st.jwt GET path Nothing ]
foldp (DeviceStatusResponse ds) st =
  noEffects $ st # deviceStatus .~ Just ds



view :: State -> HTML Event
view st@{ deviceStatus: Just (DeviceStatus s) }  = do
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

view { deviceStatus: Nothing } = do
  h3 $ text "Device not loaded"
  button
    #! onClick (const DeviceStatusRequest)
    $ text "reload"
