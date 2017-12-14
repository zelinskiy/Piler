module Utils.Other where

import Prelude

import Data.String(take, drop, length)

trimAny :: String -> String
trimAny = drop 1 <<< (\s -> take (length s - 1) s)
