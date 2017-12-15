module HomePage ( State(..)
                , Event(..)
                , Effects(..)
                , foldp
                , view
                , init
                ) where

import Prelude

import Data.Foldable(for_)
import Data.Either(Either(..))
import Data.Maybe (Maybe(..))
import Control.Monad.Aff (attempt, delay)
import Data.Time.Duration (Milliseconds(Milliseconds))

import DOM (DOM)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.DOM.Events (DOMEvent, onClick, targetValue)
import Pux.DOM.HTML.Attributes (style)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (span, button, br, p, hr, h3, h6, input)
import Text.Smolder.HTML.Attributes (type', value)
import Text.Smolder.Markup ((!), (#!), text)
import CSS (color, red, green)
import CSS.Geometry(marginRight, width)
import CSS.Size(em)

import Network.HTTP.Affjax (AJAX)
import Control.Monad.Eff.Console (CONSOLE)

import Utils.Request(getJsonAuth, JWT)

import Types.Device

serverRoot :: String
serverRoot = "http://localhost:8080/"

type State = { jwt :: JWT
             , prompting :: Boolean
             , error :: String }

data Event
  = GetAllMeds
  | ShowDebug String
    
  | SignOutRequest
  | SubscribeRequest
  | SeizeTheMeansRequest
    
  | AskSecretCode
  | SaySecretCode DOMEvent
  | DontSaySecretCode
  

type Effects fx =
  ( ajax :: AJAX
  , console :: CONSOLE
  , dom :: DOM | fx)

init :: JWT -> State
init jwt = { jwt: jwt
           , prompting: false
           , error: "" }


foldp :: forall fx. Event
      -> State
      -> EffModel State Event (Effects fx)
      
foldp GetAllMeds st@{jwt: jwt} = onlyEffects st
  [ do
       let path = serverRoot <> "private/medicament/all/"
       res <- attempt $ getJsonAuth jwt path
       pure $ Just $ ShowDebug $ case res of
         Left e -> show e
         Right r -> show r.response
  ]

foldp (ShowDebug m) st =
  { state: st { error = m }
  , effects:
    [ delay (Milliseconds 3000.0) $> Just (ShowDebug "") ]
  }

-- Navigation events

foldp SignOutRequest _ = noEffects (init "")
  
foldp SubscribeRequest st =
  onlyEffects st [ pure $ Just AskSecretCode ]
  
foldp SeizeTheMeansRequest st@{jwt: jwt} = onlyEffects st
  [ do
       let path = serverRoot <> "private/admin/seize/the/means"
       res <- attempt $ getJsonAuth jwt path
       pure $ Just $ ShowDebug $ case res of
         Left e -> show e
         Right r -> show r.response
  ]

foldp AskSecretCode s = noEffects $ s{prompting = true}
foldp DontSaySecretCode s = noEffects $ s{prompting = false}
foldp (SaySecretCode ev) s = onlyEffects s
  [ pure $ Just DontSaySecretCode
  , pure $ Just $ ShowDebug "Not implemented" ]

view :: State -> HTML Event
view s@{ error: e } = do
  navigationView s
  hr
  p
    ! style (color red)
    $ text e
  deviceView s  
  button
    ! type' "button"
    #! onClick (const GetAllMeds)
    $ text "Test"
  

deviceView :: State -> HTML Event
deviceView s = do
  h3 $ text "Your device status:"
  p $ text $ "ip: " <> "127.0.0.1:1234"
  h6 $ text "Storage:"
  for_ storage renderStorage
  where
    storage = [ DeviceStorage { quantity: 2, medicamentId: 1 }
              , DeviceStorage { quantity: 1, medicamentId: 2 } ]
    renderStorage (DeviceStorage { quantity: q, medicamentId: m }) =
      p $ text $ show q <> " of " <> show m

navigationView :: State -> HTML Event
navigationView st | st.prompting = do
  input
    ! style (width $ 20.0 # em)
    ! type' "text"
    ! value "Contact admin for secret code please"
  button
    #! onClick SaySecretCode
    $ text "✓" 
  button
    #! onClick (const DontSaySecretCode)
    $ text "✗" 
  
navigationView _ = do
  span
    ! style do
        color green
        marginRight (10.0 # em)
    $ text "You are logged in."
  button
    #! onClick (const SignOutRequest)
    $ text "Log out"
  button
    #! onClick (const SubscribeRequest)
    $ text "Subscribe"
  button
    ! style (color red)
    #! onClick (const SeizeTheMeansRequest)
    $ text "☭"

