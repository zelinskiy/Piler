module Utils.Other where

import Prelude

import Data.Array(cons, uncons)
import Data.String(take, drop, length)
import Data.Maybe(Maybe(Just, Nothing))
import Data.Either(Either, either)

                      
trimAny :: String -> String
trimAny = drop 1 <<< (\s -> take (length s - 1) s)

-- This Maybe is actually always Just
eitherEvents :: forall a b c.
                (a -> c) -> (b -> c) -> Either a b -> Maybe c
eitherEvents e1 e2 = either (Just <<< e1) (Just <<< e2)

mapi :: forall a b. (Int -> a -> b) -> Array a -> Array b
mapi = helper 0
  where
    helper i f xs =
      case uncons xs of
           Just { head: x, tail: xs } ->
             f i x `cons` helper (i+1) f xs
           Nothing -> []
