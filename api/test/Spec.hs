{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

--import Api.Main (app)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import qualified MedicamentSpec
import qualified TreatmentSpec
import qualified AuthJWTSpec

--TODO: register test@mail.com before & unregister after
main :: IO ()
main = do
  hspec $ do
    AuthJWTSpec.spec
    MedicamentSpec.spec
    TreatmentSpec.spec
    
