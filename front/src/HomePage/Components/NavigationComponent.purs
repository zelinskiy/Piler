module HomePage.Components.NavigationComponent
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
import Data.Lens.Getter((^.))

import Config(serverRoot)
import Utils.Request(request)
import Utils.Other(eitherConsoleEvent, constUnit)
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
  | AskSecretCode
  | SaySecretCode DOMEvent
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
    in eitherConsoleEvent (constUnit  MyselfRequest)
       =<< request st.jwt GET path Nothing ]

foldp AskSecretCode st = noEffects $ st {prompting = true}
foldp DontSaySecretCode st = noEffects $ st {prompting = false}
foldp (SaySecretCode ev) st =
  { state: st # prompting .~ false 
  , effects:
    [ let path = serverRoot
               <> "private/user/upgrade/SubscribeSilver/"
               <> targetValue ev
      in eitherConsoleEvent (constUnit MyselfRequest)
         =<< request st.jwt GET path Nothing ]
  }

foldp MyselfRequest st = onlyEffects st
  [ let path = serverRoot <> "private/user/me/"
    in eitherConsoleEvent MyselfResponse
       =<< request st.jwt GET path Nothing ]

foldp (MyselfResponse me) st =
  noEffects $ st { me = me }






view :: State -> HTML Event
view st | st.prompting = do
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

view st = do
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
