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
import Control.Monad.Eff.Class (liftEff)

import Pux (EffModel, mapEffects, mapState)
import Pux.DOM.HTML (HTML, mapEvent)
import DOM.HTML.History (DocumentTitle(..), URL(..), pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import DOM.HTML (window)
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup (text)

import Routes as Route
import LoginPage as LoginPage
import HomePage as HomePage

-- TODO:
-- make navbar component

type State = { currentRoute :: Route.Route
             , loginState :: LoginPage.State
             , homeState :: HomePage.State }

data Event
  = LoginPageEvent LoginPage.Event
  | HomePageEvent HomePage.Event
  | Navigate Route.Route

type Effects fx =
  HomePage.Effects
  (LoginPage.Effects
   (history :: HISTORY | fx))

init :: State
init = { currentRoute: Route.Login
       , loginState: LoginPage.init
       , homeState: HomePage.init "JWT" }

foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)
  
foldp (Navigate route) st =
  { state: st { currentRoute = route }
  , effects: [
      liftEff do
        h <- history =<< window
        let url = Route.toString route
        pushState (toForeign {}) (DocumentTitle "") (URL url) h
        pure Nothing
    ]
  }

foldp (LoginPageEvent ev) st =
  LoginPage.foldp ev st.loginState
  # mapEffects LoginPageEvent
  # mapState stateTransform
  where
    stateTransform s =
      let st' = st { loginState = s  }
      in case ev of
        LoginPage.SignInResult (Right jwt) ->
          st' { currentRoute = Route.Home
              , homeState = st'.homeState { jwt = jwt }}
        _ -> st'
        
  
foldp (HomePageEvent ev) st =
  HomePage.foldp ev st.homeState
  # mapEffects HomePageEvent
  # mapState stateTransform
  where
    stateTransform s =
      let st' = st { homeState = s  }
      in case ev of
        HomePage.SignOutRequest ->
          st' { currentRoute = Route.Login }
        _ -> st'

  
view :: State -> HTML Event
view st =
  case st.currentRoute of
    Route.Login ->
      mapEvent LoginPageEvent
      $ LoginPage.view st.loginState
    Route.Home ->
      mapEvent HomePageEvent
      $ HomePage.view st.homeState
    Route.NotFound -> h1 $ text "Not Found"
