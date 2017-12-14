module Routes where

import Control.Alt ((<|>))
import Control.Apply ((<*), (*>))
import Data.Functor ((<$), (<$>))
import Data.Function (($))
import Data.Maybe (fromMaybe)
import Pux.Router (param, router, lit, int, end)

import Utils.Request (JWT)

data Route
  = Login
  | Medicament JWT
  | NotFound

match :: String -> Route
match url = fromMaybe NotFound $ router url $
  Login <$ end
  <|>
  Medicament <$> (lit "medicaments" *> param "jwt") <* end
