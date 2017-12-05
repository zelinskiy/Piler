{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON

import Network.HTTP.Types.Method
import Network.HTTP.Types.Header
import Data.String.Conversions
import Network.Wai.Test hiding (request)
import Data.Aeson
import Test.Hspec.Wai.Internal
import Data.List
import qualified Data.ByteString as BS
import Network.Wai

import App (app)
import JsonModel (RegisterData(..), Login(Login))

import qualified MedicamentSpec
import qualified TreatmentSpec
import qualified AuthJWTSpec
import qualified UserSpec


testUser =
  RegisterData "test@mail.com" "pass" "127.0.0.1"

getJwt :: WaiSession BS.ByteString
getJwt = BS.drop 7 <$> snd <$> head
    <$> filter (\(n, _) -> n == hAuthorization) 
    <$> simpleHeaders
    <$> request methodPost "/public/jwt/login" []
    (encode $ Login (email testUser) (pass testUser))

main :: IO ()
main = do
  hspec $ with app $ do
    UserSpec.registerSpec testUser
    AuthJWTSpec.spec
    --MedicamentSpec.spec jwt
    --TreatmentSpec.spec jwt    
    UserSpec.unregisterSpec getJwt
  
