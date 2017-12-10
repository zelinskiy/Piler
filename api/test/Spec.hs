{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.Hspec.Wai.Internal

import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Data.String.Conversions
import Network.Wai.Test hiding (request)
import Data.Aeson
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS (c2w, w2c)
import System.Process
import Data.Word8
import Network.Wai

import App (app)
import JsonModel (RegisterData(..), Login(Login))

import qualified MedicamentSpec
import qualified TreatmentSpec
import qualified AuthJWTSpec
import qualified UserSpec
import qualified ShoppingSpec


testUser =
  RegisterData "test@mail.com" "pass" "127.0.0.1"
testLogin =
  Login (email testUser) (pass testUser)

getJwt :: WaiSession BS.ByteString
getJwt = BS.takeWhile (/= BS.c2w ';') <$> BS.drop 11
  <$> snd <$> head
  <$> filter (\(n, c) -> n == "Set-Cookie"
               && "JWT-Cookie" `isPrefixOf` cs c) 
  <$> simpleHeaders
  <$> request methodPost "/public/jwt/login"
      [(hContentType,"application/json")]
      (encode testLogin)

main :: IO ()
main = hspec $ with app $ do
    UserSpec.registerSpec testUser    
    AuthJWTSpec.spec testLogin
    MedicamentSpec.spec getJwt
    TreatmentSpec.spec getJwt
    ShoppingSpec.spec getJwt
    UserSpec.unregisterSpec getJwt
    
