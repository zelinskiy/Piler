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
    idDevice Int
    ip String
    userId String
    Primary idDevice
    Foreign User fkUserId userId

User json
    email String
    password String
    deviceId Int
    Primary email
    Foreign Device fkDeviceId deviceId
    deriving Eq Show

TreatmentPlan json
    idTreatmentPlan Int
    userId String
    Primary idTreatmentPlan
    Foreign User fkUserId userId
    deriving Eq Show

TreatmentPlanRow json
    idTreatmentPlanRow Int
    at UTCTime
    medicamentId Int
    treatmentPlan Int
    Primary idTreatmentPlanRow
    Foreign Medicament fkMedicamentId medicamentId
    Foreign TreatmentPlan fkTreatmentPlan treatmentPlan

Medicament json
    idMedicament Int
    name String
    diameter Int
    height Int
    description Text Maybe
    Primary idMedicament
    deriving Eq Show

|]

  
