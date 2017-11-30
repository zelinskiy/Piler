{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}

module Model where

import Database.Persist
import Database.Persist.TH
import Data.Aeson
import Data.Aeson.TH
import Data.Text
import GHC.Generics
import Data.Time.Clock(UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Device json
    ip String
    userId UserId
User json
    email String
    password String
    deviceId DeviceId
    Primary email
    deriving Eq Show
TreatmentPlan json
    userId UserId
    deriving Eq Show
TreatmentPlanRow json
    at UTCTime
    medicamentId MedicamentId
    treatmentPlan TreatmentPlanId
Medicament json
    name String
    diameter Int
    height Int
    description Text Maybe
    deriving Eq Show
|]
