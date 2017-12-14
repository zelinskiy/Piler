module Routes where

import Control.Alt ((<|>))
import Control.Apply ((<*))
import Data.Functor ((<$))
import Data.Function (($))
import Data.Maybe (fromMaybe)
import Pux.Router (router, lit, end)

data Route
  = Login
  | Medicament
  | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
  Login <$ end
  <|>
  Medicament <$ (lit "medicaments") <* end
