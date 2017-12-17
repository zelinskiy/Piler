module LoginPage( State(..)
                , Event(..)
                , Effects(..)
                , foldp
                , view
                , init
                , Mode
                ) where

import Prelude

import Data.Either(Either(..), either)
import Data.Maybe (Maybe(..))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (attempt)

import Data.Argonaut (encodeJson)

import DOM (DOM)
import Pux (EffModel, noEffects, onlyEffects)
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
import Types.Login (Login, defaultLogin, email, pass, ip)
import Utils.Request(postJson, JWT)
import Utils.Other(trimAny)

-----------
-- STATE --
-----------

data Mode = LoginMode | RegisterMode 
derive instance eqMode :: Eq Mode

type State = { login :: Login
             , mode :: Mode
             , jwt :: Maybe JWT
             , error :: String }

login :: Lens' State Login
login = prop (SProxy :: SProxy "login")

mode :: Lens' State Mode
mode = prop (SProxy :: SProxy "mode")

jwt :: Lens' State (Maybe JWT)
jwt = prop (SProxy :: SProxy "jwt")

error :: Lens' State String
error = prop (SProxy :: SProxy "error")

data Event
  = SignInRequest
  | SignInResult (Either String JWT)
  | SignUpRequest
  | SignUpResult (Maybe String)
  | ReplaceLogin Login
  | SetMode Mode


type Effects fx =
  ( ajax :: AJAX
  , console :: CONSOLE
  , dom :: DOM | fx)

init :: State
init = { login: defaultLogin
       , mode: LoginMode
       , jwt: Nothing
       , error: "" }

------------
-- UPDATE --
------------

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
  
foldp SignInRequest st = onlyEffects st
  [ do
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

foldp (SetMode m) st = noEffects $ st { mode = m }

foldp (SignInResult (Left err)) st =
  { state: st # error .~ err
  , effects: [ liftEff $ Console.error err *> pure Nothing ] }
  
foldp (SignUpResult Nothing) st = onlyEffects st
  [ liftEff (Console.log "Registered") $> Just SignInRequest ]

foldp (SignUpResult (Just e)) st =
  noEffects $ st { error = e }

foldp SignUpRequest st = onlyEffects st
  [ let path = serverRoot <> "public/user/register/"
    in attempt (postJson path (encodeJson st.login))
       >>= either (Just <<< show) (const Nothing)
       >>> SignUpResult
       >>> Just
       >>> pure
  ]

----------
-- VIEW --
----------

view :: State -> HTML Event
view { login: l
     , error: e
     , mode: mode } = do
  p ! style do
    color green
    $ text "Welcome to The Piler!"
  p ! style (color red) $ text e

  let fields = email .| flip (*>) br
               <> (pass <<< asPassword) .| flip (*>) br
      fields' = if mode == RegisterMode
                then fields <> (ip .| flip (*>) br)
                else fields
  form l fields' ReplaceLogin

  case mode of
    LoginMode -> do
      button 
        ! type' "button" 
        #! onClick (const SignInRequest) $ text "Sign In"
      button 
        ! type' "button" 
        #! onClick (const (SetMode RegisterMode)) $ text "Register"
    RegisterMode -> do
      button 
        ! type' "button" 
        #! onClick (const SignUpRequest) $ text "Sign Up"
      button 
        ! type' "button" 
        #! onClick (const (SetMode LoginMode)) $ text "Return"
    
  
