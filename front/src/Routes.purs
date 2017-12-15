module Routes where

import Control.Alt ((<|>))
import Control.Apply ((<*))
import Data.Functor ((<$))
import Data.Function (($))
import Data.Maybe (fromMaybe)
import Pux.Router (router, lit, end)

data Route
  = Login
  | Home
  | NotFound


toString :: Route -> String
toString Login = "/login"
toString Home = "/"
toString NotFound = "/void" 


match :: String -> Route
match url = fromMaybe NotFound $ router url $
  Login <$ end
  <|>
  Home <$ (lit "home") <* end
