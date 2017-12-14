module Login.App where

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

newtype State = State
                { login :: Login
                , jwt :: Maybe JWT
                , error :: String }

data Event
  = SignInRequest
  | SignInResult (Either String JWT)
  | EmailChange DOMEvent
  | PasswordChange DOMEvent
-- Second part
  | GetAllMeds
  | ShowAllMeds String
  | SignOutRequest
    
defaultState :: State
defaultState =
  State { login: defaultLogin
        , jwt: Nothing
        , error: "" }

init :: State
init = defaultState

foldp :: forall fx. Event
      -> State
      -> EffModel State Event
      ( ajax :: AJAX
      , console :: CONSOLE
      , dom :: DOM | fx)

foldp (EmailChange ev) (State st@{ login: Login l }) =
  noEffects $ State $ st { login = Login $ l { email = targetValue ev } }
  
foldp (PasswordChange ev) (State st@{ login: Login l }) =
  noEffects $ State $ st { login = Login $ l { pass = targetValue ev } }
  
foldp (SignInResult (Left err)) (State st) =
  { state: State $ st { error = err }
  , effects: [ liftEff $ error err *> pure Nothing ] }
  
foldp (SignInResult (Right jwt)) (State st) =
  { state: State $ st { jwt = Just jwt }
  , effects: [ liftEff $ log jwt *> pure Nothing ] }
  
foldp GetAllMeds st@(State {jwt: Just jwt}) =
  { state: st
  , effects: [ do
      let path = serverRoot <> "private/medicament/all/"
      res <- attempt $ getJsonAuth jwt path
      pure $ Just $ ShowAllMeds $ case res of
        Left e -> show e
        Right r -> show r.response
    ]
  }

foldp GetAllMeds (State st@{jwt: Nothing})  =
  noEffects $ State $ st { error = "Not logged" }

foldp SignInRequest (State st) =  
  { state: State st
  , effects: [ do
      let path = serverRoot <> "public/jwt/login/"
      res <- attempt $ postJson path (encodeJson st.login)
      let ret = case res of
            Left e -> Left $ show e
            Right r -> if r.status == StatusCode 200
                       then Right $ trimAny $ r.response
                       else Left $ "[" <> show r.status <> "] "
                            <> r.response
      pure $ Just $ SignInResult ret
    ]
  }

foldp SignOutRequest _ = noEffects init

foldp (ShowAllMeds m) (State st) =
  noEffects $ State $ st { error = m }

view :: State -> HTML Event
view (State { login: Login { email: email, pass:pass }, error: e, jwt: Just _}) = do
  p ! style (color green) $ text "You are logged in."
  button ! type' "button" #! onClick (const SignOutRequest) $ text "Log out"
  br
  button ! type' "button" #! onClick (const GetAllMeds) $ text "Test"
  p ! style (color red) $ text e
  
view (State { login: Login { email: email, pass:pass }, error: e, jwt: Nothing }) = do
  p ! style do
    color green
    $ text "Welcome to The Piler!"
  p ! style (color red) $ text e
  
  form ! name "signin" #! onSubmit (const SignInRequest) $ do
    input ! type' "text" ! value email #! onChange EmailChange
    br
    input ! type' "password" ! value pass #! onChange PasswordChange
    br
    button ! type' "button" #! onClick (const SignInRequest) $ text "Sign In"
