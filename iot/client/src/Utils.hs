{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Utils where

import Data.Monoid
import Data.List
import Network.HTTP.Simple
import System.Hardware.Serialport
import System.IO
import System.Environment
import Data.String.Conversions

import Types

get jwt route
  = httpLBS
  $ setRequestMethod "GET"
  $ addRequestHeader "Authorization" ("Bearer " <> jwt)
  $ parseRequest_ route

postJson jwt route body
  = httpLBS
  $ setRequestMethod "POST"
  $ addRequestHeader "Authorization" ("Bearer " <> jwt)
  $ addRequestHeader "Content-Type" "application/json"
  $ setRequestBodyJSON body
  $ parseRequest_ route

getConfig :: IO Configuration
getConfig = withFile "config.txt" ReadMode $ \h -> do
  [cp, tty] <- getArgs
  [e, p, u] <- lines <$> hGetContents h
  h <- openSerial tty defaultSerialSettings
      { commSpeed = CS9600 }
  return Configuration
    { serverUrl  = u
    , clientHost = "localhost"
    , clientPort = read cp
    , devicePort = h
    , token      = "Put JWT here!"
    , userEmail  = e
    , userPass   = p }

getJWT :: Configuration -> IO JWT
getJWT c = do
  let route = serverUrl c <> "/public/jwt/login"
      login = Login (userEmail c) (userPass c)
      chop = drop 1
           . takeWhile (/= ';')
           . dropWhile (/= '=')
      findJWT (h:hs)
        | "JWT-Cookie" `isPrefixOf` h = chop h
        | otherwise = findJWT hs
      findJWT [] = error "cannot find auth cookie"
  cs <$> findJWT <$> map cs
     <$> getResponseHeader "Set-Cookie"
     <$> postJson "" route login
