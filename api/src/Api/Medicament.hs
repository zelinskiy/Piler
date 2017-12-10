{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Api.Medicament
    ( API
    , server
    ) where

import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Servant

import Model
import Utils

type API =
         "all"
      :> Get '[JSON] [Entity Medicament]
    :<|> "add"
      :> ReqBody '[JSON] Medicament
      :> Post '[JSON] (Key Medicament)
    :<|> "delete"
      :> Capture "id" (Key Medicament)
      :> Delete '[JSON] ()

server :: ConnectionPool -> Entity User -> Server API
server p me =
       allMedicaments
  :<|> addMedicament
  :<|> deleteMedicament
  where
    allMedicaments = exPool p $ selectList [] []
    addMedicament med
      | medicamentHeight med <= 0 = throwError $ err400
          { errBody = "Height should be positive" }
      | medicamentDiameter med <= 0 = throwError $ err400
          { errBody = "Diameter should be positive" } 
    addMedicament med = exPool p $ insert med
    deleteMedicament = exPool p . delete

