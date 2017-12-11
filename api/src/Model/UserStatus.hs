{-# LANGUAGE TemplateHaskell #-}
module Model.UserStatus where

import Database.Persist.TH
import Prelude
import GHC.Generics
import Data.Aeson

data UserStatus
  = Normal
  | Silver
  | Admin
    deriving (Show, Read, Eq, Ord, Generic)
derivePersistField "UserStatus"

instance ToJSON UserStatus
instance FromJSON UserStatus
