{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric #-}

module Model where

import Database.Persist
import Database.Persist.TH
import Data.Aeson
import Data.Aeson.TH
import Data.Text
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Medicament json
    name String
    diameter Int
    height Int
    description Text Maybe
    deriving Eq Show
|]

data User = User { email :: String, password :: String }
  deriving (Eq, Show, Generic)
  
