module HomePage.Components.TreatmentComponent
       (Event(..), foldp , view ) where

import Prelude

import Data.Either(Either(..))
import Data.Foldable(for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Monad.Eff.Class (liftEff)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (p, h3, h4)
import Text.Smolder.Markup (text)
import Data.HTTP.Method (Method (GET))
import Control.Monad.Eff.Console (errorShow) as Console
import Data.Lens.Setter((.~))

import Config(serverRoot)
import Utils.Request(request)
import Utils.Other(eitherConsoleEvent)
import Types.Medicament
import Types.DeviceStorage(DeviceStorage(..))
import Types.TreatmentPlan
import Types.TreatmentPlanRow
import Types.FullTreatmentPlan(FullTreatmentPlan(..))

import HomePage.State
import HomePage.Effects(Effects)


data Event
  = TreatmentsRequest
  | TreatmentsResponse (Array FullTreatmentPlan)





foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)
foldp TreatmentsRequest st = onlyEffects st
  [ let path = serverRoot <> "private/treatment/my/full/"
    in eitherConsoleEvent TreatmentsResponse
       =<< request st.jwt GET path Nothing ]
foldp (TreatmentsResponse tps) st =
  noEffects $ st # treatment .~ tps





view :: State -> HTML Event
view { treatment: [] } =
  h3 $ text "No treatment plans"
view st@{ treatment: tps } = do
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
  

