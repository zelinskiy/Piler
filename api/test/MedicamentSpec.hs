{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module MedicamentSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Data.ByteString.Internal
import Network.Wai.Test (simpleBody)
import Data.String.Conversions
import Data.Aeson
import Network.Wai

import Model

spec :: WaiSession ByteString -> SpecWith Application
spec getJwt = do
  describe "medicaments" $ do
    it "returns all items" $ do
      jwt <- getJwt
      getAuth jwt (root `mappend` "/all")
        `shouldRespondWith` 200
        
    it "adds items" $ do
      jwt <- getJwt
      addTestItemRequest jwt "TestItem" 1 1
        `shouldRespondWith` 200
        
    it "adds and deletes items" $ do
      jwt <- getJwt
      mid <- cs <$> simpleBody <$> addTestItemRequest jwt "TestItem" 1 1
      deleteAuth jwt (root `mappend` "/delete/" <> mid)
        `shouldRespondWith` 200
        
    it "refuses to add item with zero diameter" $ do
      jwt <- getJwt
      addTestItemRequest jwt "TestItem" 0 1
        `shouldRespondWith` 400
  where
    root = "/private/medicament"
    defHeaders jwt =
      [(hContentType,"application/json")
      , (hAuthorization, "Bearer " <> jwt)]
    getAuth jwt route = request methodGet route (defHeaders jwt) ""
    deleteAuth jwt route = request methodDelete route (defHeaders jwt) ""
    addTestItemRequest jwt n d h =
      request methodPost
      (root `mappend` "/add")
      (defHeaders jwt)
      (encode $ Medicament
        { medicamentName = n
        , medicamentDiameter = d
        , medicamentHeight = h
        , medicamentDescription = Nothing})
    

    
