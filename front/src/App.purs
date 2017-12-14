module App ( State(..)
           , Event(..)
           , Effects(..)
           , foldp
           , view
           , init
           ) where

import Prelude

import Data.Either(Either(..))

import Pux (EffModel, mapEffects, mapState)
import Pux.DOM.HTML (HTML, mapEvent)
import Text.Smolder.HTML (hr)

import Login (State, Event(SignInResult), Effects, foldp, view, init) as LoginApp
import Medicaments (State, Event, Effects, foldp, view, init) as MedicamentsApp

type State = { loginState :: LoginApp.State
             , medicamentsState :: MedicamentsApp.State }

data Event
  = LoginEvent LoginApp.Event
  | MedicamentsEvent MedicamentsApp.Event

type Effects fx = MedicamentsApp.Effects (LoginApp.Effects fx)

init :: State
init = { loginState: LoginApp.init
       , medicamentsState: MedicamentsApp.init "" }

foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)
      
foldp (LoginEvent ev) st =
  LoginApp.foldp ev st.loginState
  # mapEffects LoginEvent
  # mapState stateTransform
  where
    stateTransform s =
      let st' = st { loginState = s }
      in case ev of
        LoginApp.SignInResult (Right jwt) -> 
         st' { medicamentsState = st.medicamentsState {jwt = jwt} }
        _ -> st'
  
foldp (MedicamentsEvent ev) st =
  MedicamentsApp.foldp ev st.medicamentsState
  # mapEffects MedicamentsEvent
  # mapState (\s -> st { medicamentsState = s  })
  
view :: State -> HTML Event
view st = do
  mapEvent LoginEvent $ LoginApp.view st.loginState
  hr
  mapEvent MedicamentsEvent $ MedicamentsApp.view st.medicamentsState
