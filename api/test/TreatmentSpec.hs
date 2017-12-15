{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module TreatmentSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai

import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Data.ByteString.Internal
import Network.Wai.Test (simpleBody)
import Data.String.Conversions
import Data.Aeson
import Network.Wai
import Data.Time.Clock
import Database.Persist.Sqlite
import Data.Maybe

import Model

spec :: WaiSession ByteString -> SpecWith Application
spec getJwt = do
  describe "treatment" $ do
    it "returns all plans" $ do
      jwt <- getJwt
      getAuth jwt (root <> "/my/plans")
        `shouldRespondWith` 200

    it "returns all rows" $ do
      jwt <- getJwt
      getAuth jwt (root <> "/my/rows")
        `shouldRespondWith` 200

    it "returns full plans" $ do
      jwt <- getJwt
      getAuth jwt (root <> "/my/full")
        `shouldRespondWith` 200
    
    it "adds treatment plan" $ do
      jwt <- getJwt
      getAuth jwt (root <> "/new/plan")
        `shouldRespondWith` 200
        
    it "adds and deletes plan" $ do
      jwt <- getJwt
      pid <- cs <$> simpleBody
        <$> getAuth jwt (root <> "/new/plan")
      deleteAuth jwt (root <> "/delete/plan/" <> pid)
        `shouldRespondWith` 200

    it "adds rows" $ do
      jwt <- getJwt
      pid <- fromMaybe (error "plan not found")
        . decode
        . simpleBody
        <$> getAuth jwt (root <> "/new/plan")
      med <- fromMaybe (error "med not found")
        . fmap entityKey
        . (listToMaybe =<<)
        . decode
        . simpleBody
        <$> getAuth jwt ("/private/medicament/all")
      
      now <- liftIO getCurrentTime
      let when = addUTCTime (2 * 10^12) now
      postAuth jwt
        (root <> "/new/row")
        (encode $ TreatmentPlanRow when 1 med pid)
        `shouldRespondWith` 200
    
  where
    root = "/private/treatment"
    defHeaders jwt =
      [ (hContentType,"application/json")
      , (hAuthorization, "Bearer " <> jwt)]
    getAuth jwt route = request methodGet route
                        (defHeaders jwt) ""
    deleteAuth jwt route = request methodDelete route (defHeaders jwt) ""
    postAuth jwt route body =
      request methodPost route (defHeaders jwt) body
    


    
