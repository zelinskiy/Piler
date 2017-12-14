module Utils.Request where

import Prelude

import Data.Either(Either(..))
import Data.Maybe (Maybe(..))

import Data.HTTP.Method (Method (POST, GET))

import Data.Argonaut (Json)

import Network.HTTP.Affjax (Affjax, URL, affjax, defaultRequest)
import Network.HTTP.RequestHeader(RequestHeader(..))


type JWT = String

get_ :: forall e. URL -> Affjax e Unit
get_ u = affjax $ defaultRequest { url = u }

postJson :: forall e. URL -> Json -> Affjax e String
postJson u d =
  affjax $ defaultRequest
    { method = Left POST
    , url = u
    , content = Just d }

getJsonAuth :: forall e. JWT -> URL -> Affjax e Json
getJsonAuth jwt u =
  let bearer = "Bearer " <> jwt
  in affjax $ defaultRequest
    { method = Left GET
    , url = u
    , headers = [RequestHeader "Authorization" bearer]
    , content = Nothing :: Maybe Unit }
