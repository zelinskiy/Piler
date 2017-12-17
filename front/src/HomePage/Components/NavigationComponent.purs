module HomePage.Components.NavigationComponent
       (Event(..), foldp , view ) where

import Prelude

import Data.Tuple
import Data.Int (toNumber)
import Data.String(joinWith)
import Data.Foldable(for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Control.Monad.Aff (delay)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Control.Bind((=<<))
import Pux (EffModel, noEffects, onlyEffects, mapEffects)
import Pux.DOM.Events (DOMEvent, onClick, targetValue, onChange)
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
import Data.Lens.Getter((^.))

import Config(serverRoot)
import Utils.Request(request, request')
import Utils.Other(eitherConsoleEvent)
import Types.Medicament
import Types.User
import Types.DeviceStatus
import Types.DeviceStorage(DeviceStorage(..))
import Types.TreatmentPlan
import Types.TreatmentPlanRow
import Types.FullTreatmentPlan(FullTreatmentPlan(..))

import HomePage.State
import HomePage.Effects(Effects)

data Event
  = SignOutRequest
  | SubscribeRequest
    
  | SeizeTheMeansRequest
    
  | GenerateSecretCodeRequest
  | GenerateSecretCodeResponse String
    
  | AskSecretCode
  | UpdateSecretCode DOMEvent
  | SaySecretCode
  | DontSaySecretCode
    
  | MyselfRequest
  | MyselfResponse User
  


foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)

foldp SignOutRequest _ = noEffects (init "")
  
foldp SubscribeRequest st =
  onlyEffects st [ pure $ Just AskSecretCode ]
  
foldp SeizeTheMeansRequest st = onlyEffects st
  [ let path = serverRoot <> "private/admin/seize/the/means"
    in request' st.jwt GET path Nothing
       $> Just MyselfRequest ]

foldp AskSecretCode st@{ prompt: Tuple _ k } =
  noEffects $ st { prompt = Tuple true k }
foldp DontSaySecretCode st@{ prompt: Tuple _ k } =
  noEffects $ st { prompt = Tuple false k }
foldp SaySecretCode st = onlyEffects st
  [ let path = serverRoot
               <> "private/user/upgrade/SubscribeSilver/"
               <> snd st.prompt
    in request' st.jwt GET path Nothing
       $> Just MyselfRequest
  , pure $ Just DontSaySecretCode]
  
foldp (UpdateSecretCode ev) st =
  noEffects $ st # prompt .~ Tuple true (targetValue ev) 
  
foldp MyselfRequest st = onlyEffects st
  [ let path = serverRoot <> "private/user/me/"
    in eitherConsoleEvent MyselfResponse
       =<< request st.jwt GET path Nothing ]

foldp (MyselfResponse me) st =
  noEffects $ st { me = me }

foldp GenerateSecretCodeRequest st = onlyEffects st
  [ let path = serverRoot
               <> "private/admin/keys/generate/SubscribeSilver/"
    in eitherConsoleEvent GenerateSecretCodeResponse
       =<< request st.jwt GET path Nothing ]

foldp (GenerateSecretCodeResponse k) st =
  noEffects $ st { prompt = Tuple true k }




view :: State -> HTML Event
view st@{ prompt: Tuple true p } = do
  input
    ! style (width $ 20.0 # em)
    ! type' "text"
    #! onChange UpdateSecretCode
    ! value p
  button
    #! onClick (const SaySecretCode)
    $ text "✓" 
  button
    #! onClick (const DontSaySecretCode)
    $ text "✗" 

view st@{ prompt: Tuple false _ } = do
  let username = st ^. me ^. email
      stat = st ^. me ^. status
  span
    ! style do
        color green
        marginRight (10.0 # em)
    $ text ("Logged as " <> username <> " (" <> stat <> ")")
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
  when (stat == "Admin") do
    button
      #! onClick (const GenerateSecretCodeRequest)
      $ text "mk key"
