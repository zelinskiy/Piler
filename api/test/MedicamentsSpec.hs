{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module MedicamentsSpec (spec) where

import MainApi (app)

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Data.ByteString.Internal
import Network.Wai.Test (simpleBody)
import Data.String.Conversions

--import Model

spec :: Spec
spec = with app $ do
  describe "medicaments" $ do
    it "returns all items" $ do
      get "/medicaments/all" `shouldRespondWith` 200
    it "adds items" $ do      
      addTestItemRequest "TestItem" 1 1 `shouldRespondWith` 200
    it "adds and deletes items" $ do
      mid <- cs <$> simpleBody <$> addTestItemRequest "TestItem" 1 1
      delete ("/medicaments/delete/" `mappend` mid)
        `shouldRespondWith` 200
    it "refuses to add item with zero diameter" $ do      
      addTestItemRequest "TestItem" 0 1 `shouldRespondWith` 400
    

addTestItemRequest n d h =
  request methodPost
    "/medicaments/add"
    [(hContentType,"application/json")]       
    (cs $ "{\"name\":" ++ show n ++ ","
    ++ "\"diameter\":" ++ show d ++ ","
    ++ "\"height\"  :" ++ show h ++ "}")
    
