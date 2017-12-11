{-# LANGUAGE TemplateHaskell #-}

module Model.SecretKeyPurpose where

import Database.Persist.TH
import Prelude
import GHC.Generics
import Data.Aeson
import Text.Read
import qualified Data.Text as T
import Web.HttpApiData
import Test.QuickCheck

import Data.DeriveTH

data SecretKeyPurpose
  = SubscribeSilver
    deriving (Show, Read, Eq, Generic)
derivePersistField "SecretKeyPurpose"

instance ToJSON SecretKeyPurpose
instance FromJSON SecretKeyPurpose

instance FromHttpApiData SecretKeyPurpose where
  parseUrlPiece p =
    case readEither (T.unpack p) of
      Left e -> Left (T.pack e)
      Right x -> Right x
      
instance ToHttpApiData SecretKeyPurpose where
  toUrlPiece = T.pack . show

derive makeArbitrary ''SecretKeyPurpose

toFromHttpApi_prop :: SecretKeyPurpose -> Bool
toFromHttpApi_prop kp =
  parseUrlPiece (toUrlPiece kp) == Right kp

