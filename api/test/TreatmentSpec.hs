{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module TreatmentSpec (spec) where

import Api.Main (app)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Data.ByteString.Internal
import Network.Wai.Test (simpleBody)
import Data.String.Conversions
import Data.Aeson

import Model

spec :: Spec
spec = with app $ do
  describe "treatment" $ do
    it "returns my plans" $ do
      getAuth (root `mappend` "/my")
        `shouldRespondWith` 200
        
    it "adds treatment plan" $ do      
      getAuth (root `mappend` "/new/plan")
        `shouldRespondWith` 200
        
    it "adds and deletes plan" $ do
      pid <- cs <$> simpleBody <$> getAuth (root `mappend` "/new/plan")
      deleteAuth (root `mappend` "/delete/plan/" `mappend` pid)
        `shouldRespondWith` 200
    
    
  where
    root = "/private2/treatment"
    defHeaders =
      [(hContentType,"application/json")
      , ("email", "user1@mail.com")
      , ("password", "pass")]
    getAuth route = request methodGet route defHeaders ""
    deleteAuth route = request methodDelete route defHeaders ""
    postAuth route body =
      request methodDelete route defHeaders body
    addTestItemRequest n d h =
      request methodPost
      (root `mappend` "/add")
      defHeaders
      (encode $ Medicament
        { medicamentName = n
        , medicamentDiameter = d
        , medicamentHeight = h
        , medicamentDescription = Nothing})
    


    
