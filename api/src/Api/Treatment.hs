module Api.Treatment (API, server) where

import Database.Persist.Sqlite
import Servant
import Data.Maybe
import Control.Monad.Trans.Reader

import Model
import JsonModel (FullTreatmentPlan(..))
import Utils

type API =
         "my"
      :>      ("full"
            :> Get '[JSON] [FullTreatmentPlan]
          :<|> "plans"
            :> Get '[JSON] [Entity TreatmentPlan]
          :<|> "rows"
            :> Get '[JSON] [Entity TreatmentPlanRow])
    :<|> "new"
      :>      ("plan"            
            :> Get '[JSON] (Key TreatmentPlan)
          :<|> "row"
            :> ReqBody '[JSON] TreatmentPlanRow
            :> Post '[JSON] (Key TreatmentPlanRow))
    :<|> "delete"
      :>      ("plan"
            :> Capture "id" (Key TreatmentPlan)
            :> Delete '[JSON] ()
          :<|> "row"
            :> Capture "id" (Key TreatmentPlanRow)
            :> Delete '[JSON] ())
    :<|> "update"
      :>      ("plan"
            :> Capture "id" (Key TreatmentPlan)
            :> ReqBody '[JSON] TreatmentPlan
            :> Post '[JSON] ()
          :<|> "row"
            :> Capture "id" (Key TreatmentPlanRow)
            :> ReqBody '[JSON] TreatmentPlanRow
            :> Post '[JSON] ())
      
      
-- balance parenthesis /new/plan -> plan/new
server :: PrivateServer API
server =
       (myFullTreatmentPlans
        :<|> myTreatmentPlans
        :<|> myTreatmentPlanRows)
  -- TODO: Check that I own these plans
  :<|> (addTreatmentPlan
        :<|> addTreatmentPlanRow)
  :<|> (deleteTreatmentPlan
        :<|> deleteTreatmentPlanRow)
  :<|> (updateTreatmentPlan
        :<|> updateTreatmentPlanRow)
  where
    myDevice = ask >>= \me -> db $
      fromJust <$> fmap entityKey <$> selectFirst
        [DeviceUserId ==. entityKey me] []
    
    myTreatmentPlans = do
      did <- myDevice
      db $ selectList [TreatmentPlanDeviceId ==. did] []
      
    myTreatmentPlanRows = do
      plans <- map entityKey <$> myTreatmentPlans
      db $
        selectList [TreatmentPlanRowTreatmentPlanId <-. plans] []
        
    myFullTreatmentPlans = do
      plans <- myTreatmentPlans
      rows <- myTreatmentPlanRows
      let filt p r =
            treatmentPlanRowTreatmentPlanId (entityVal r)
            == entityKey p
      return $ map (\p -> FullTreatmentPlan
            { treatmentPlan = p
            , treatmentPlanRows = filter (filt p) rows })
        plans
        
    addTreatmentPlan = do
      did <- myDevice
      db $ insert $ TreatmentPlan did
      
    addTreatmentPlanRow = db . insert
    
    deleteTreatmentPlan = db . delete
    
    deleteTreatmentPlanRow = db . delete
    
    updateTreatmentPlan pid plan =
      db $ replace pid plan
      
    updateTreatmentPlanRow rid row =
      db $ replace rid row
