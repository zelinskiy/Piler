module HomePage.Components.TreatmentComponent
       (Event(..), foldp , view ) where

import Prelude

import Data.Either(Either(..))
import Data.Foldable(for_)
import Data.Enum(fromEnum)
import Data.String(joinWith, fromCharArray, toCharArray)
import Data.Array(dropEnd)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, wrap, unwrap)

import Control.Monad.Eff.Class (liftEff)
import Data.Lens.Setter((.~))
import Data.Lens(Lens', (^?), lens)
import Data.Lens.Getter((^.))

import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.HTML (HTML)
import Pux.DOM.Events (onClick, onChange, targetValue)
import Pux.DOM.HTML.Attributes (style)
import Pux.DOM.HTML.Attributes as A
import Pux.Form(class Render, render, field, form, (.|))
import CSS.Geometry(marginRight)
import CSS.Size(em)

import Text.Smolder.HTML (p, h3, h4, span, button, br)
import Text.Smolder.HTML as HTML
import Text.Smolder.HTML.Attributes (type', value)
import Text.Smolder.Markup ((!), (#!), text)
import Data.HTTP.Method (Method (GET, POST, DELETE))

import Control.Monad.Eff.Console (errorShow) as Console

import Data.Argonaut(encodeJson)
import Data.DateTime(DateTime(DateTime), day,
                     month, year, hour, minute)
import Data.JSDate(toUTCString, fromDateTime)

import Config(serverRoot)
import Utils.Request(request, request')
import Utils.Other(eitherConsoleEvent, parseDateTime)
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

newtype DateTimeString = DateTimeString String
derive instance newtypeDateTimeString :: Newtype DateTimeString _

-- We pass around DateTime's as something like
-- 2017-12-01T00:00:00Z
-- But html rejects it with the trailing Z,
-- so here is a workaround
instance renderDateTime :: Render DateTimeString where
  render a = 
    HTML.input
      ! type' "datetime-local"
      ! value (dropLast (unwrap a))
      #! onChange (wrap <<< flip (<>) "Z" <<< targetValue)
    where
      dropLast = fromCharArray <<< dropEnd 1 <<< toCharArray

asDateTimeString :: Lens' String DateTimeString
asDateTimeString = cast :: Lens' String DateTimeString
  where 
    cast :: forall a b.(Newtype b a)=> Lens' a b
    cast = lens wrap $ const unwrap
    

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
view { treatment: [] } =
  h3 $ text "No treatment plans"
view st@{ treatment: tps } = do
  h3 do
    span $ text "Treatment plans"
    button
      ! type' "button" 
      #! onClick (const AddTreatmentPlan)
      $ text "Add"
  renderNewRowForm
  for_ tps renderPlan
  where
    renderNewRowForm = do
      let btn = button
                ! type' "button" 
                #! onClick (const AddTreatmentRow)
                $ text "Add"
          fields = field (at <<< asDateTimeString)
                   <> field quantity
                   <> field treatmentPlanId
                   <> (medicamentId
                       .| flip (*>) btn)
      form st.newTreatmentRow fields ReplaceNewTreatmentRow
    
    renderPlan (FullTreatmentPlan p) = do
      let pid = getPlanId p.treatmentPlan

      h3 do
        span $ text $ "Treatment plan #" <> show pid
        button
          ! type' "button" 
          #! onClick (const (DeleteTreatmentPlan pid))
          $ text "X"
        
      for_ p.treatmentPlanRows renderPlanRow
      
    
    getPlanId (TreatmentPlan p) = p.id

    showDate d =
      joinWith "." (map show [ fromEnum $ day d
                             , fromEnum $ month d
                             , fromEnum $ year d])
    showTime t =
      "(" <> show (fromEnum (hour t))
      <> ":" <> show (fromEnum (minute t)) <> ")"

    showDateTime (DateTime d t) =
      showDate d <> " " <> showTime t
      
    renderPlanRow (TreatmentPlanRow r) = do      
      button
        #! onClick (const $ DeleteTreatmentRow r.id)
        $ text "Delete"
      span $ text $
        "Take " <> show r.quantity <> " of "
        <> fromMaybe "Unknown" (getMedicamentName r.medicamentId)
        <> " at " <> showDateTime (parseDateTime r.at)
        <> " (Stored: " <> show (howMuchStored r.medicamentId) <> ")"
      br  
                 
    getMedicamentName =
      getMedicament st
      >>> map (\(Medicament m) -> m.name)
          
    howMuchStored =
      getMedicamentStored st
      >>> map (\(DeviceStorage ds) -> ds.quantity)
      >>> fromMaybe 0
  

