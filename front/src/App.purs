module App ( State(..)
           , Event(..)
           , Effects(..)
           , foldp
           , view
           , init
           ) where

import Prelude

import Data.Either(Either(..))
import Data.Maybe (Maybe(..))
import Data.Foreign (toForeign)
import Data.Functor(map)
import Control.Monad.Eff.Class (liftEff)

import Pux (EffModel, noEffects, mapEffects, mapState)
import Pux.DOM.Events (DOMEvent)
import Pux.DOM.HTML (HTML, mapEvent)
import DOM.Event.Event (preventDefault)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import DOM.HTML (window)
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup (text)

import Routes (Route(..), match)
import Login (State, Event(SignInResult), Effects, foldp, view, init) as LoginApp
import Medicaments (State, Event(SignOutRequest), Effects, foldp, view, init) as MedicamentsApp

-- TODO:
-- make navbar component

type State = { currentRoute :: Route
             , loginState :: LoginApp.State
             , medicamentsState :: MedicamentsApp.State }

data Event
  = LoginEvent LoginApp.Event
  | MedicamentsEvent MedicamentsApp.Event
  | PageView Route
  | Navigate String DOMEvent

type Effects fx =
  MedicamentsApp.Effects
  (LoginApp.Effects
   (history :: HISTORY | fx))

init :: State
init = { currentRoute: Login
       , loginState: LoginApp.init
       , medicamentsState: MedicamentsApp.init "" }

foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)

foldp (Navigate url ev) st =
  { state: st
  , effects: [
      liftEff do
        preventDefault ev
        h <- history =<< window
        pushState (toForeign {}) (DocumentTitle "") (URL url) h
        pure $ Just $ PageView (match url)
    ]
  }
  
foldp (PageView route) st =
  noEffects $ st { currentRoute = route }

foldp (LoginEvent ev) st =
  LoginApp.foldp ev st.loginState
  # mapEffects LoginEvent
  # mapState stateTransform
  where
    stateTransform s =
      let st' = st { loginState = s  }
      in case ev of
        LoginApp.SignInResult (Right jwt) ->
          st' { currentRoute = Medicament
              , medicamentsState =
                st'.medicamentsState { jwt = jwt }}
        _ -> st'
        
  
foldp (MedicamentsEvent ev) st =
  MedicamentsApp.foldp ev st.medicamentsState
  # mapEffects MedicamentsEvent
  # mapState stateTransform
  where
    stateTransform s =
      let st' = st { medicamentsState = s  }
      in case ev of
        MedicamentsApp.SignOutRequest ->
          st' { currentRoute = Login }
        _ -> st'

  
view :: State -> HTML Event
view st =
  case st.currentRoute of
    Login ->
      mapEvent LoginEvent
      $ LoginApp.view st.loginState
    Medicament ->
      mapEvent MedicamentsEvent
      $ MedicamentsApp.view st.medicamentsState
    NotFound -> h1 $ text "Not Found"
