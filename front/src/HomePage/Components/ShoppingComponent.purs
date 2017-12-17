module HomePage.Components.ShoppingComponent
       (Event(..), foldp , view ) where

import Prelude

import Data.Either(Either(..))
import Data.Foldable(for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Monad.Eff.Class (liftEff)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (DOMEvent, onClick)
import Pux.DOM.HTML.Attributes (style)
import Text.Smolder.HTML (p, h3, h4, span, button, br)
import Text.Smolder.HTML.Attributes (type')
import Text.Smolder.Markup ((!), (#!), text)
import Data.HTTP.Method (Method (GET, POST, DELETE))
import Control.Monad.Eff.Console (errorShow) as Console
import Data.Lens.Setter((.~))
import Pux.Form(field, form, (.|))
import CSS.Geometry(marginRight)
import CSS.Size(em)
import Data.Lens((^?))
import Data.Lens.Getter((^.))
import Data.Argonaut(encodeJson)

import Config(serverRoot)
import Utils.Request(request, request')
import Utils.Other(eitherConsoleEvent)
import Types.Medicament
import Types.User
import Types.DeviceStorage(DeviceStorage(..))
import Types.TreatmentPlan as TP
import Types.TreatmentPlan
import Types.TreatmentPlanRow
import Types.FullTreatmentPlan(FullTreatmentPlan(..))

import HomePage.State
import HomePage.Effects(Effects)


data Event
  = ShoppingListsRequest
  | ShoppingListsResponse (Array FullTreatmentPlan)
  | ReplaceNewListName DOMEvent
  | AddShoppingListRow
  | DeleteShoppingListRow Int  
  | AddShoppingList
  | DeleteShoppingList Int


foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)

foldp _ st = noEffects st
      
view :: State -> HTML Event
view st | st ^. me ^. status == "Normal" = do
  h3 $ text "Subscribe to unlock this feature"
view st = do
  h3 $ text "Shopping Lists"
  p $ text "Not implemented"
