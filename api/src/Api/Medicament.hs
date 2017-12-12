module Api.Medicament (API, server) where

import Control.Monad.Trans.Reader
import Database.Persist.Sqlite
import Servant

import Model
import Utils

type API =
         "all"
      :> Get '[JSON] [Entity Medicament]
    :<|> "my"
      :> Get '[JSON] [Entity Medicament]
    :<|> "add"
      :> ReqBody '[JSON] Medicament
      :> Post '[JSON] (Key Medicament)
    :<|> "delete"
      :> Capture "id" (Key Medicament)
      :> Delete '[JSON] ()
    
server :: PrivateServer API
server = allMedicaments :<|> myMedicaments
  :<|> enterRole (>= Admin) closed
  where
    closed = addMedicament :<|> deleteMedicament
    allMedicaments = db $ selectList [] []
    addMedicament med
      | medicamentHeight med <= 0 = throwError $ err400
          { errBody = "Height should be positive" }
      | medicamentDiameter med <= 0 = throwError $ err400
          { errBody = "Diameter should be positive" } 
    addMedicament med = db $ insert med
    deleteMedicament = db . delete
    myMedicaments = ask >>= \me -> db $ do
      Just d <- selectFirst [DeviceUserId ==. entityKey me] []
      mids <- map deviceStorageMedicamentId <$> map entityVal
        <$> selectList [DeviceStorageDeviceId ==. entityKey d] []
      selectList [MedicamentId <-. mids] []

