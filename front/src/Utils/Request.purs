module Utils.Request where

import Prelude

import Data.Either(Either(..), either)
import Data.Maybe (Maybe(..))
import Control.Monad.Aff (Aff, attempt)
import Data.HTTP.Method (Method (POST, GET))
import Data.Argonaut (Json, class DecodeJson, decodeJson)
import Network.HTTP.Affjax (Affjax, URL, affjax, defaultRequest)
import Network.HTTP.RequestHeader(RequestHeader(..))
import Network.HTTP.Affjax (AJAX)

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


request :: forall e fx. DecodeJson e
         => JWT
         -> Method
         -> URL
         -> Maybe Json
         -> Aff (ajax :: AJAX | fx) (Either String e)
request jwt method path j =
  let bearer = "Bearer " <> jwt
      req = defaultRequest
        { method = Left method
        , url = path
        , headers = [RequestHeader "Authorization" bearer]
        , content = j }
  in affjax req # attempt >>=
     pure <<< either (Left <<< show) (decodeJson <<< _.response) 

    
