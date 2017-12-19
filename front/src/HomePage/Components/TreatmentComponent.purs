module HomePage.Components.TreatmentComponent
       (Event(..), foldp , view ) where

import Prelude 

import Data.Either(Either(..))
import Data.Foldable(for_)
import Data.Enum(fromEnum)
import Data.String(joinWith)
import Data.Maybe (Maybe(..), fromMaybe)

import Control.Monad.Eff.Class (liftEff)
import Data.Lens.Setter((.~))
import Data.Lens(Lens', (^?), (^..), lens)
import Data.Lens.Getter((^.))
import Data.Lens as L

import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (DOMEvent, onClick, onChange, targetValue)
import Pux.DOM.HTML.Attributes (style)
import Pux.Form (field, form, (.|))
import CSS.Geometry(marginRight, width)
import CSS.Size(em)

import Text.Smolder.HTML (p, h3, h4, span, button, br)
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes (type')
import Text.Smolder.HTML.Attributes as A
import Text.Smolder.Markup ((!), (#!), text)
import Data.HTTP.Method (Method (GET, POST, DELETE))

import Control.Monad.Eff.Console (errorShow) as Console

import Data.Argonaut(encodeJson)
import Data.DateTime(DateTime(DateTime), day,
                     month, year, hour, minute)

import Config(serverRoot)
import Utils.Request(request, request')
import Utils.Other(eitherConsoleEvent, parseDateTime)
import Utils.Form

import Types.Medicament as M
import Types.Medicament
import Types.DeviceStorage(DeviceStorage(..))
import Types.TreatmentPlan as TP
import Types.TreatmentPlan
import Types.TreatmentPlanRow
import Types.FullTreatmentPlan(FullTreatmentPlan(..), treatmentPlan)

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
    in request' st.jwt DELETE path Nothing
       $> Just TreatmentsRequest ]
  
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
view st@{ treatment: tps } = do
  h3 do
    button
      ! type' "button" 
      #! onClick (const AddTreatmentPlan)
      $ text "Add"
    span $ text "Treatment plans"
    
  renderNewRowForm
  for_ tps renderPlan
  
  where
    renderNewRowForm = do
      let btn = button
                ! type' "button" 
                #! onClick (const AddTreatmentRow)
                $ text "Add"
          showPid i = "Plan #" <> show i
          pids = tps <#> L.view treatmentPlan >>> L.view TP.id
          mids = st.medicaments <#> L.view M.id
          quantFieldModifier h =
            h ! (style $ width $ 5.0 # em) ! A.min "1"
          -- NOTE: This is `hacked` by rewriting the original
          -- asDropdown. But there must be some clever
          -- solution with custom Lens'
          fields = field (at <<< asDateTimeString)
                   <> (quantity .| quantFieldModifier)
                   <> field (treatmentPlanId
                       <<< asDropdown showPid pids)
                   <> (medicamentId
                       <<< asDropdown getMedicamentName mids
                       .| flip (*>) btn)
      form st.newTreatmentRow fields ReplaceNewTreatmentRow
    
    renderPlan (FullTreatmentPlan p) = do
      let pid = p.treatmentPlan ^. TP.id

      h3 do
        button
          ! type' "button" 
          #! onClick (const (DeleteTreatmentPlan pid))
          $ text "✗"
        span $ text $ "Treatment plan #" <> show pid
        
      for_ (p.treatmentPlanRows) renderPlanRow
        
      
    renderPlanRow (TreatmentPlanRow r) = do
      let showDate d =
            joinWith "." (map show [ fromEnum $ day d
                                   , fromEnum $ month d
                                   , fromEnum $ year d])
          showTime t =
            "(" <> show (fromEnum (hour t))
            <> ":" <> show (fromEnum (minute t)) <> ")"

          showDateTime (DateTime d t) =
            showDate d <> " " <> showTime t
      button
        #! onClick (const $ DeleteTreatmentRow r.id)
        $ text "✗"
      span $ text $
        "Take " <> show r.quantity <> " of "
        <> getMedicamentName r.medicamentId
        <> " at " <> showDateTime (parseDateTime r.at)
        <> " (Stored: " <> show (howMuchStored r.medicamentId) <> ")"
      br  
    
    getMedicamentName =
      getMedicament st
      >>> map (\(Medicament m) -> m.name)
      >>> fromMaybe "Unknown"
          
    howMuchStored =
      getMedicamentStored st
      >>> map (\(DeviceStorage ds) -> ds.quantity)
      >>> fromMaybe 0
  

