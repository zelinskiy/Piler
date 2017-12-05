{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module TreatmentSpec (spec) where

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
  describe "treatment" $ do
    it "returns my plans" $ do
      jwt <- getJwt
      getAuth jwt (root `mappend` "/my")
        `shouldRespondWith` 200
        
    it "adds treatment plan" $ do
      jwt <- getJwt
      getAuth jwt (root `mappend` "/new/plan")
        `shouldRespondWith` 200
        
    it "adds and deletes plan" $ do
      jwt <- getJwt
      pid <- cs <$> simpleBody <$> getAuth jwt (root `mappend` "/new/plan")
      deleteAuth jwt (root `mappend` "/delete/plan/" `mappend` pid)
        `shouldRespondWith` 200
    
  where
    root = "/private/treatment"
    defHeaders jwt =
      [ (hContentType,"application/json")
      , (hAuthorization, "Bearer " <> jwt)]
    getAuth jwt route = request methodGet route (defHeaders jwt) ""
    deleteAuth jwt route = request methodDelete route (defHeaders jwt) ""
    postAuth jwt route body =
      request methodDelete route (defHeaders jwt) body
    addTestItemRequest jwt n d h =
      request methodPost
      (root `mappend` "/add")
      (defHeaders jwt)
      (encode $ Medicament
        { medicamentName = n
        , medicamentDiameter = d
        , medicamentHeight = h
        , medicamentDescription = Nothing})
    


    
