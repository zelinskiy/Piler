module Medicaments ( State(..)
                   , Event(..)
                   , Effects(..)
                   , foldp
                   , view
                   , init
                   ) where

import Prelude

import Data.Either(Either(..))
import Data.Maybe (Maybe(..))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (attempt)

import Data.Argonaut (encodeJson)

import DOM (DOM)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent, onSubmit, onChange, onClick, targetValue)
import Pux.DOM.HTML.Attributes (style)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (button, form, input, br, p)
import Text.Smolder.HTML.Attributes (name, type', value)
import Text.Smolder.Markup ((!), (#!), text)
import CSS (color, red, green)

import Network.HTTP.StatusCode(StatusCode(..))
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Console (CONSOLE, error, log)

import Types.Login (Login(..), defaultLogin)
import Utils.Request(postJson, getJsonAuth, JWT)
import Utils.Other(trimAny)

serverRoot :: String
serverRoot = "http://localhost:8080/"

type State = { jwt :: JWT
             , error :: String }

data Event
  = GetAllMeds
  | ShowAllMeds String
  | SignOutRequest

type Effects fx =
  ( ajax :: AJAX
  , console :: CONSOLE
  , dom :: DOM | fx)

init :: JWT -> State
init jwt = { jwt: jwt
           , error: "" }


foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)
      
foldp GetAllMeds st@{jwt: jwt} =
  { state: st
  , effects: [ do
      let path = serverRoot <> "private/medicament/all/"
      res <- attempt $ getJsonAuth jwt path
      pure $ Just $ ShowAllMeds $ case res of
        Left e -> show e
        Right r -> show r.response
    ]
  }

foldp SignOutRequest _ = noEffects (init "")

foldp (ShowAllMeds m) st =
  noEffects $ st { error = m }

view :: State -> HTML Event
view { error: e } = do
  p ! style (color green) $ text "You are logged in."
  button ! type' "button" #! onClick (const SignOutRequest) $ text "Log out"
  br
  button ! type' "button" #! onClick (const GetAllMeds) $ text "Test"
  p ! style (color red) $ text e
  
