{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module MedicamentApi
    ( API
    , server
    ) where

import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Servant

import Model

type API =
         "all"
      :> Get '[JSON] [Entity Medicament]
    :<|> "add"
      :> ReqBody '[JSON] Medicament
      :> Post '[JSON] (Key Medicament)
    :<|> "delete"
      :> Capture "id" (Key Medicament)
      :> Delete '[JSON] ()

server :: ConnectionPool -> User -> Server API
server pool user =
       allMedicaments
  :<|> addMedicament
  :<|> deleteMedicament
  where
    runPool a = liftIO $ flip runSqlPersistMPool pool $ a
    allMedicaments =
      liftIO $ flip runSqlPersistMPool pool $ selectList [] []
    addMedicament med = do
      if all (>0) [medicamentHeight med, medicamentDiameter med]
      then liftIO $ flip runSqlPersistMPool pool $ insert med
      else throwError $ err400 {
        errBody = "Diameter and height should be positive" }
    deleteMedicament mid =
      runPool $ deleteWhere [MedicamentId ==. mid]

