module HomePage.Components.TreatmentComponent
       (Event(..), foldp , view ) where

import Prelude

import Data.Either(Either(..))
import Data.Foldable(for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Monad.Eff.Class (liftEff)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick)
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
import Types.DeviceStorage(DeviceStorage(..))
import Types.TreatmentPlan as TP
import Types.TreatmentPlan
import Types.TreatmentPlanRow
import Types.FullTreatmentPlan(FullTreatmentPlan(..))

import HomePage.State
import HomePage.Effects(Effects)


data Event
  = TreatmentsRequest
  | TreatmentsResponse (Array FullTreatmentPlan)
  | ReplaceNewTreatmentRow TreatmentPlanRow
  | DeleteTreatmentRow Int
  | AddTreatmentRow
  | AddTreatmentPlan
  | DeleteTreatmentPlan Int



foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)
      
foldp TreatmentsRequest st = onlyEffects st
  [ let path = serverRoot <> "private/treatment/my/full/"
    in eitherConsoleEvent TreatmentsResponse
       =<< request st.jwt GET path Nothing ]
  
foldp (TreatmentsResponse tps) st =
  noEffects $ st # treatment .~ tps
  
foldp (DeleteTreatmentRow rid) st = onlyEffects st
  [ let path = serverRoot
               <> "private/treatment/delete/row/"
               <> show rid
    in request' st.jwt DELETE path Nothing $> Just TreatmentsRequest ]
  
foldp (ReplaceNewTreatmentRow r) st =
  noEffects $ st # newTreatmentRow .~ r

foldp AddTreatmentRow st = onlyEffects st
  [ let path = serverRoot <> "private/treatment/new/row/"
        d = Just (encodeJson st.newTreatmentRow)
    in request' st.jwt POST path d
       $> Just TreatmentsRequest ]

foldp AddTreatmentPlan st = onlyEffects st
  [ let path = serverRoot <> "private/treatment/new/plan/"
    in request' st.jwt GET path Nothing
       $> Just TreatmentsRequest ] 

foldp (DeleteTreatmentPlan pid) st = onlyEffects st
  [ let path = serverRoot
               <> "private/treatment/delete/plan/"
               <> show pid
    in request' st.jwt DELETE path Nothing
       $> Just TreatmentsRequest ] 

view :: State -> HTML Event
view { treatment: [] } =
  h3 $ text "No treatment plans"
view st@{ treatment: tps } = do
  h3 do
    span
      ! style (marginRight (2.0 # em))
      $ text "Treatment plans"
    button
      ! type' "button" 
      #! onClick (const AddTreatmentPlan)
      $ text "Add"
  for_ tps renderPlan
  where    
    renderPlan (FullTreatmentPlan p) = do
      let pid = getPlanId p.treatmentPlan

      h3 do
        span
          ! style (marginRight (2.0 # em))
          $ text $ "Treatment plan #" <> show pid
        button
          ! type' "button" 
          #! onClick (const (DeleteTreatmentPlan pid))
          $ text "X"
        
      for_ p.treatmentPlanRows renderPlanRow
      
      let btn = button
                ! type' "button" 
                #! onClick (const AddTreatmentRow)
                $ text "Add"
          fields = field at
                   <> field quantity
                   <> (medicamentId
                       .| flip (*>) btn)
          r = st ^. newTreatmentRow # treatmentPlanId .~ pid
      form r fields ReplaceNewTreatmentRow
      
      
      
    getPlanId (TreatmentPlan p) = p.id
    
    renderPlanRow (TreatmentPlanRow r) = do
      
      button
        ! style (marginRight (2.0 # em))
        #! onClick (const $ DeleteTreatmentRow r.id)
        $ text "Delete"
      span $ text $
        "Take " <> show r.quantity <> " of "
        <> fromMaybe "Unknown" (getMedicamentName r.medicamentId)
        <> " at " <> r.at
        <> " (Stored: " <> show (howMuchStored r.medicamentId) <> ")"
      br  
                 
    getMedicamentName =
      getMedicament st
      >>> map (\(Medicament m) -> m.name)
          
    howMuchStored =
      getMedicamentStored st
      >>> map (\(DeviceStorage ds) -> ds.quantity)
      >>> fromMaybe 0
  

