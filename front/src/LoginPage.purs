module LoginPage( State(..)
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
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML.Attributes (style)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (button, br, p)
import Text.Smolder.HTML.Attributes (type')
import Text.Smolder.Markup ((!), (#!), text)
import CSS (color, red, green)
  
import Network.HTTP.StatusCode(StatusCode(..))
import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Console (log, error) as Console

import Pux.Form(field, form, (.|))
import Pux.Form.Render (asPassword)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Symbol (SProxy(..))
import Data.Lens.Setter((.~))

import Config(serverRoot)
import Types.Login (Login, defaultLogin, email, pass)
import Utils.Request(postJson, JWT)
import Utils.Other(trimAny)

type State = { login :: Login
             , jwt :: Maybe JWT
             , error :: String }

login :: Lens' State Login
login = prop (SProxy :: SProxy "login")

jwt :: Lens' State (Maybe JWT)
jwt = prop (SProxy :: SProxy "jwt")

error :: Lens' State String
error = prop (SProxy :: SProxy "error")

data Event
  = SignInRequest
  | SignInResult (Either String JWT)
  | ReplaceLogin Login


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

foldp (ReplaceLogin l) st = noEffects $ (login .~ l) st
  
foldp (SignInResult (Left err)) st =
  { state: st # error .~ err
  , effects: [ liftEff $ Console.error err *> pure Nothing ] }
  
foldp (SignInResult (Right j)) st =
  { state: st # jwt .~ Just j
  , effects:
    [ liftEff $ Console.log j *> pure Nothing ]
  }
  
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
view { login: l
     , error: e } = do
  p ! style do
    color green
    $ text "Welcome to The Piler!"
  p ! style (color red) $ text e

  let fields = email .| flip (*>) br
               <> field (pass <<< asPassword)
  form l fields ReplaceLogin
  button 
    ! type' "button" 
    #! onClick (const SignInRequest) $ text "Sign In"
  
