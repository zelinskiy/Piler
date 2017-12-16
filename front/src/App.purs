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

import Pux (EffModel, mapEffects, mapState, noEffects)
import Pux.DOM.HTML (HTML, mapEvent)
import DOM.HTML.History(DocumentTitle(..),
                        URL(..),
                        pushState)
import DOM.HTML.Types (HISTORY)
import DOM.HTML.Window (history)
import DOM.HTML (window)
import Text.Smolder.HTML (h1)
import Text.Smolder.Markup (text)

import Routes as Route
import LoginPage as LoginPage
import HomePage as HomePage

-- TODO:

type State = { currentRoute :: Route.Route
             , loginState :: LoginPage.State
             , homeState :: HomePage.State }

data Event
  = Init
  | LoginPageEvent LoginPage.Event
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

foldp Init st = noEffects st

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

foldp (LoginPageEvent ev) st = res
  where
    res0 = LoginPage.foldp ev st.loginState
        # mapEffects LoginPageEvent
        # mapState (\s -> st { loginState = s })    
    res = case ev of
      LoginPage.SignInResult (Right jwt) ->
        { state: res0.state
              { currentRoute = Route.Home
              , homeState = res0.state.homeState { jwt = jwt }}
        , effects: res0.effects <>
          [pure $ Just $ HomePageEvent $ HomePage.Init] }
      _ -> res0
        
  
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
