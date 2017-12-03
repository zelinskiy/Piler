{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module MedicamentSpec (spec) where

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
  describe "medicaments" $ do
    it "returns all items" $ do
      getAuth (root `mappend` "/all")
        `shouldRespondWith` 200
        
    it "adds items" $ do      
      addTestItemRequest "TestItem" 1 1
        `shouldRespondWith` 200
        
    it "adds and deletes items" $ do
      mid <- cs <$> simpleBody <$> addTestItemRequest "TestItem" 1 1
      deleteAuth (root `mappend` "/delete/" `mappend` mid)
        `shouldRespondWith` 200
        
    it "refuses to add item with zero diameter" $ do      
      addTestItemRequest "TestItem" 0 1
        `shouldRespondWith` 400
  where
    root = "/private2/medicament"
    defHeaders =
      [(hContentType,"application/json")
      , ("email", "user1@mail.com")
      , ("password", "pass")]
    getAuth route = request methodGet route defHeaders ""
    deleteAuth route = request methodDelete route defHeaders ""
    addTestItemRequest n d h =
      request methodPost
      (root `mappend` "/add")
      defHeaders
      (encode $ Medicament
        { medicamentName = n
        , medicamentDiameter = d
        , medicamentHeight = h
        , medicamentDescription = Nothing})
    

    
