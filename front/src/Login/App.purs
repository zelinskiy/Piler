module Login.App where

import Prelude
import Control.Monad.Eff.Class (liftEff)
import Data.Either(Either(..), either)
import Data.Maybe (Maybe(..), isJust)
import Data.Foldable (find)
import Control.Monad.Aff (attempt)
import Data.String(stripPrefix, stripSuffix, Pattern(..), joinWith)


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
import Data.HTTP.Method (Method (POST))
import Network.HTTP.Affjax (Affjax, AJAX, URL, affjax, post_, defaultRequest)
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

init :: State
init = State
       { login:
            Login { email: "user1@mail.com"
                  , pass: "pass" }
       , error: "empty" }

get_ :: forall e. URL -> Affjax e Unit
get_ u = affjax $ defaultRequest { url = u }

postJson :: forall e. URL -> Json -> Affjax e Unit
postJson u d =
  affjax $ defaultRequest
    { method = Left POST
    , url = u
    , headers = [ RequestHeader "Access-Control-Allow-Origin" "*"
                , RequestHeader "Content-Type" "application/json" ]
    , content = Just d }

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
foldp (SignInRequest _) (State st) =  
  { state: State st
  , effects: [ do
      let path = "http://localhost:8080/public/jwt/login"
      res <- attempt $ postJson path (encodeJson st.login) 
      let mbBearer = either (Left <<< show) allHeaders res
      pure $ Just $ SignInResult $ case mbBearer of
        Right jwt -> Right jwt
        --Right Nothing -> Left "Could not find or parse JWT"
        Left err -> Left err
    ]
  }
  where
    allHeaders = Right <<< joinWith ";" <<< map responseHeaderName
                 <<< (\r -> r.headers)
    --extractJwt = Right <<< join <<< find isJust
    --    <<< map pred <<< (\r -> r.headers)
    pred r =
        responseHeaderName r #
        stripPrefix (Pattern "") >>=
        stripSuffix (Pattern "")
    pred _ = Nothing
  
  

view :: State -> HTML Event
view (State { login: Login { email: email, pass:pass }, error: e }) = do
  p ! style do
    color green
    $ text "Welcome!"
  p ! style (color red) $ text e
  form ! name "signin" #! onSubmit SignInRequest $ do
    input ! type' "text" ! value email #! onChange EmailChange
    p $ text email
    input ! type' "password" ! value pass #! onChange PasswordChange
    br
    button ! type' "button" #! onClick SignInRequest $ text "Sign In"

