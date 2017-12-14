module Login.App where

import Prelude
import Data.Array(length) as A
import Control.Monad.Eff.Class (liftEff)
import Data.Either(Either(..), either)
import Data.Maybe (Maybe(..), isJust)
import Data.Foldable (find)
import Control.Monad.Aff (attempt)
import Data.String(stripPrefix, stripSuffix, Pattern(..), joinWith, take, drop, length)
import Data.MediaType (MediaType(..))
import Data.Foreign(Foreign)
import Data.Semigroup((<>))

import DOM (DOM)
import Pux (EffModel, noEffects)
import Pux.DOM.Events (DOMEvent, onSubmit, onChange, onClick, targetValue)
import Pux.DOM.HTML.Attributes (style)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (button, form, input, br, p)
import Text.Smolder.HTML.Attributes (name, type', value)
import Text.Smolder.Markup ((!), (#!), text)
import CSS (color, red, green)
import Data.Argonaut (encodeJson, Json)
import Data.HTTP.Method (Method (POST, GET))
import Network.HTTP.Affjax (Affjax, AJAX, URL, affjax, post_, defaultRequest, get)
import Network.HTTP.RequestHeader(RequestHeader(..))
import Network.HTTP.ResponseHeader(responseHeaderName, responseHeaderValue)
import Control.Monad.Eff.Console (CONSOLE, error, logShow)

import Types.Login (Login(..))

newtype State = State
                { login :: Login
                , error :: String }
                
type JWT = String

data Event
  = SignInRequest DOMEvent
  | SignInResult (Either String JWT)
  | EmailChange DOMEvent
  | PasswordChange DOMEvent
  | TestEvent DOMEvent

init :: State
init = State
       { login:
            Login { email: "user1@mail.com"
                  , pass: "pass" }
       , error: "empty" }

get_ :: forall e. URL -> Affjax e Unit
get_ u = affjax $ defaultRequest { url = u }

postJson :: forall e. URL -> Json -> Affjax e String
postJson u d =
  affjax $ 
    { method: Left POST
    , url: u
    , headers: [ RequestHeader "Access-Control-Allow-Origin" "*"
               , Accept (MediaType "*/*")
               , ContentType (MediaType "application/json") ]
    , content: Just d
    , username: Nothing
    , password: Nothing
    , withCredentials: false }

getJsonAuth :: forall e. JWT -> URL -> Affjax e Json
getJsonAuth jwt u =
  affjax $ 
    { method: Left GET
    , url: u
    , headers: [ RequestHeader "Authorization" ("Bearer " <> jwt)
               , Accept (MediaType "*/*")
               , ContentType (MediaType "application/json") ]
    , content: Nothing :: Maybe Unit
    , username: Nothing
    , password: Nothing
    , withCredentials: false }

foldp :: forall fx. Event
      -> State
      -> EffModel State Event
      ( ajax :: AJAX
      , console :: CONSOLE
      , dom :: DOM | fx)
foldp (EmailChange ev) (State { login: Login l, error: e}) =
  noEffects $ State { login: (Login $ l { email = targetValue ev })
                    , error: e }
foldp (PasswordChange ev) (State { login: Login l, error: e}) =
  noEffects $ State { login: (Login $ l { pass = targetValue ev })
                    , error: e }
foldp (SignInResult (Left err)) (State st) =
  { state: State $ st { error = err }
  , effects: [ liftEff $ error err *> pure Nothing ]
  }
foldp (SignInResult (Right jwt)) (State st) =
  noEffects $ State $ st { error = jwt }
foldp (TestEvent _) (State st) =
  { state: State st
  , effects: [ do
      let path = "http://localhost:8080/private/medicament/all"
      res <- attempt $ getJsonAuth st.error path
      pure $ Just $ SignInResult
        $ either (Left <<< show) (Right <<< show <<< _.response) res
    ]
  }
foldp (SignInRequest _) (State st) =  
  { state: State st
  , effects: [ do
      let path = "http://localhost:8080/public/jwt/login"
      res <- attempt $ postJson path (encodeJson st.login) 
      pure $ Just $ SignInResult
        $ either (Left <<< show) (Right <<< drop 1
                                  <<< (\s -> take (length s - 1) s)
                                  <<< _.response) res
    ]
  }
  
  

view :: State -> HTML Event
view (State { login: Login { email: email, pass:pass }, error: e }) = do
  p ! style do
    color green
    $ text "Welcome!"
  p ! style (color red) $ text e
  button ! type' "button" #! onClick TestEvent $ text "Test"
  form ! name "signin" #! onSubmit SignInRequest $ do
    input ! type' "text" ! value email #! onChange EmailChange
    p $ text email
    input ! type' "password" ! value pass #! onChange PasswordChange
    br
    button ! type' "button" #! onClick SignInRequest $ text "Sign In"

