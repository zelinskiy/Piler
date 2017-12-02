{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

--import Api.Main (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import qualified MedicamentSpec
import qualified TreatmentSpec

--TODO: register before & unregister after
main :: IO ()
main = do
  hspec (MedicamentSpec.spec >> TreatmentSpec.spec)
