{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module UserSpec (registerSpec, unregisterSpec) where

import Test.Hspec
import Test.Hspec.Wai
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Data.ByteString.Internal
import Data.String.Conversions
import Data.Aeson
import Network.Wai

import JsonModel(RegisterData(..))

registerSpec :: RegisterData -> SpecWith Application
registerSpec rdata = do
  describe "User registration" $ do
    let h = [(hContentType,"application/json")]
        l = encode rdata
    it ("registers " ++ email rdata) $
      (request methodPost "/public/user/register" h l
        `shouldRespondWith` 200)

unregisterSpec :: WaiSession ByteString -> SpecWith Application
unregisterSpec getJwt = do
  describe "User unregistration" $ do
    let h jwt = [ (hContentType,"application/json")
                , (hAuthorization, "Bearer " <> jwt)]
    it "unregisters" $ do
      jwt <- getJwt
      request methodGet "/private/user/unregister" (h jwt) ""
        `shouldRespondWith` 200
    

    
