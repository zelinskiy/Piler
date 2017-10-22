{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import MainApi (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import qualified MedicamentsSpec

main :: IO ()
main = do
  hspec MedicamentsSpec.spec
