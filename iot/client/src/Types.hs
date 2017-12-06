{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Types where

import Data.Aeson
import System.Hardware.Serialport
import Data.ByteString.Internal (ByteString)
import GHC.Generics

type JWT = ByteString

data Login = Login
  { email :: String
  , pass :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Login
instance FromJSON Login

data Configuration
  = Configuration
  { serverUrl :: String
  , clientHost :: String
  , clientPort :: Int
  , devicePort :: SerialPort
  , token :: JWT
  , userEmail :: String
  , userPass :: String }

