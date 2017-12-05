{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module AuthJWTSpec (spec) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Data.ByteString.Internal
import Network.Wai.Test hiding (request)
import Data.String.Conversions
import Data.Aeson
import Test.Hspec.Wai.Internal
import Control.Monad
import Data.List
import Control.DeepSeq
import Control.Exception
import Network.Wai

import Model
import Api.AuthJWT (Login(..))

assertFailure :: String -> Session ()
assertFailure msg = msg `deepseq` liftIO (throwIO (WaiTestFailure msg))

findCookie headers cname =
  let pred (h,c) = h == "Set-Cookie"
                   && cname `isPrefixOf` cs c
  in not $ null $ filter pred headers

spec :: SpecWith Application
spec = do
  describe "JWT authentication" $ do
    let h = [(hContentType,"application/json")]
        l = encode $ Login "user1@mail.com" "pass"
    it "returns JWT & XSRF tokens" $ do
      res <- request methodPost "/public/jwt/login" h l
      WaiSession $ do
        assertStatus 204 res
        let hs = simpleHeaders res
        when (not $ findCookie hs "JWT-Cookie")
          (assertFailure "JWT token not returned")
        when (not $ findCookie hs "XSRF-TOKEN")
          (assertFailure "XSRF token not returned")        
      return ()
        
    

    
