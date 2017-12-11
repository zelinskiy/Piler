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

module Model.Model where


import Database.Persist.TH
import Data.Text
import Data.Time.Clock

import Model.UserStatus as UserStatus
import Model.SecretKeyPurpose as SecretKeyPurpose


-- TODO:
-- More expressive types (Ip, Nat, Email, ..)
-- Restrict pass to 32 chars
-- String => ByteString

share [mkPersist sqlSettings
      , mkDeleteCascade sqlSettings
      , mkMigrate "migrateAll"]
  [persistLowerCase|
Device json
    ip String
    userId UserId
    Primary ip
    deriving Eq Show
DeviceStorage json
    quantity Int
    medicamentId MedicamentId
    deviceId DeviceId 
    Primary medicamentId deviceId
    deriving Eq Show
User json
    email String
    password String
    status UserStatus
    Primary email
    deriving Eq Show
TreatmentPlan json
    deviceId DeviceId 
    deriving Eq Show
TreatmentPlanRow json
    at UTCTime
    quantity Int
    medicamentId MedicamentId
    treatmentPlanId TreatmentPlanId
    deriving Eq Show
Medicament json
    name String
    diameter Int
    height Int
    description Text Maybe
    deriving Eq Show
ShoppingList json
    name String
    userId UserId
    deriving Eq Show
ShoppingListRow json
    shoppingListId ShoppingListId
    medicamentId MedicamentId
    quantity Int
    Primary shoppingListId medicamentId
    deriving Eq Show
SecretKey json
    value String
    purpose SecretKeyPurpose
    deriving Eq Show
|]
