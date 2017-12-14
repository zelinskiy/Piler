module Login( State(..)
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
import Utils.Request(postJson, JWT)
import Utils.Other(trimAny)

serverRoot :: String
serverRoot = "http://localhost:8080/"

type State = { login :: Login
             , jwt :: Maybe JWT
             , error :: String }

data Event
  = SignInRequest
  | SignInResult (Either String JWT)
  | EmailChange DOMEvent
  | PasswordChange DOMEvent


type Effects fx =
  ( ajax :: AJAX
  , console :: CONSOLE
  , dom :: DOM | fx)

init :: State
init = { login: defaultLogin
       , jwt: Nothing
       , error: "" }

foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)
foldp (EmailChange ev) st@{ login: Login l } =
  noEffects $ st { login = Login $ l { email = targetValue ev } }
  
foldp (PasswordChange ev) st@{ login: Login l } =
  noEffects $ st { login = Login $ l { pass = targetValue ev } }
  
foldp (SignInResult (Left err)) st =
  { state: st { error = err }
  , effects: [ liftEff $ error err *> pure Nothing ] }
  
foldp (SignInResult (Right jwt)) st =
  { state: st { jwt = Just jwt }
  , effects: [ liftEff $ log jwt *> pure Nothing ] }
  
foldp SignInRequest st =  
  { state: st
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

view :: State -> HTML Event
view { login: Login { email: email, pass:pass }
     , error: e, jwt: _ } = do
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
