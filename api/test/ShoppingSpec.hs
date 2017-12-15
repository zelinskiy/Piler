{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ShoppingSpec (spec) where

import Test.Hspec hiding (pending)
import Test.Hspec.Wai
import Control.Monad.IO.Class
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Data.ByteString.Internal
import Network.Wai.Test (simpleBody)
import Data.String.Conversions
import Data.Aeson
import Network.Wai

import Database.Persist.Sqlite


import Network.Wai.Test hiding (request)
import Control.Exception
import Data.Maybe
import Control.Monad

import Model

fall :: MonadIO m => String -> m a
fall msg = liftIO (throwIO (WaiTestFailure msg))

spec :: WaiSession ByteString -> SpecWith Application
spec getJwt = do

  describe "shopping list" $ do
    it "seizes the means" $ do
      jwt <- getJwt
      getAuth jwt "/private/admin/seize/the/means"
        `shouldRespondWith` 200
    
    it "returns all lists" $ do
      jwt <- getJwt
      getAuth jwt (root <> "/list/all")
        `shouldRespondWith` 200
    
    it "adds and gets shopping list" $ do
      jwt <- getJwt
      lid <- cs <$> simpleBody
        <$> getAuth jwt (root <> "/list/new/testList")
      getAuth jwt (root <> "/list/get/" <> lid)
        `shouldRespondWith` 200      
        
    it "adds and deletes list" $ do
      jwt <- getJwt
      pid <- cs <$> simpleBody
        <$> getAuth jwt (root <> "/list/new/tetsList") 
      deleteAuth jwt (root <> "/list/delete/" <> pid)
        `shouldRespondWith` 200

    it "adds, updates and deletes list" $ do
      jwt <- getJwt
      uid <- entityKey <$> getMe jwt
      let t1 = testList uid
          t2 = anotherTestList uid
      lid <- cs <$> simpleBody
        <$> getAuth jwt
           (root <> "/list/new/" <> cs (shoppingListName t1))

      res1 <- fromJust
        <$> decode
        <$> simpleBody
        <$> getAuth jwt (root <> "/list/get/" <> lid)

      when (t1 /= res1) $ fall "t1 /= res1"
      
      postAuth jwt
        (root <> "/list/update/" <> lid)
        (encode t2)

      res2 <- fromJust
        <$> decode
        <$> simpleBody
        <$> getAuth jwt (root <> "/list/get/" <> lid)

      when (t2 /= res2) $ fall "t2 /= res2"
      
      deleteAuth jwt (root <> "/list/delete/" <> lid)
        `shouldRespondWith` 200
  
  describe "shopping list row" $ do
    it "does something good" $ do
      pending
    
  where
    root = "/private/shopping"
    defHeaders jwt =
      [ (hContentType,"application/json")
      , (hAuthorization, "Bearer " <> jwt)]
    getAuth jwt route = request methodGet route (defHeaders jwt) ""
    deleteAuth jwt route = request methodDelete route (defHeaders jwt) ""
    postAuth jwt route body =
      request methodPost route (defHeaders jwt) body
    getMe :: ByteString -> WaiSession (Entity User)
    getMe jwt  = fromJust
      <$> decode
      <$> simpleBody
      <$> getAuth jwt "/private/user/me"
    testList uid = ShoppingList "testList" uid
    anotherTestList uid = ShoppingList "anotherTestList" uid
    


    
