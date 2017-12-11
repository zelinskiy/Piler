module Api.Medicament
    ( API
    , server
    ) where

import Database.Persist.Sqlite

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

server :: PrivateServer API
server = 
       allMedicaments
  :<|> addMedicament
  :<|> deleteMedicament
  where
    
    allMedicaments = db $ selectList [] []
    addMedicament med
      | medicamentHeight med <= 0 = throwError $ err400
          { errBody = "Height should be positive" }
      | medicamentDiameter med <= 0 = throwError $ err400
          { errBody = "Diameter should be positive" } 
    addMedicament med = db (insert  med)
    deleteMedicament = db . delete

