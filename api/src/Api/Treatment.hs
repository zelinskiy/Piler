module Api.Treatment (API, server) where

import Database.Persist.Sqlite
import Servant
import Data.Maybe
import Control.Monad.Trans.Reader
import Control.Monad
import Data.List(find)
import Data.Time.Format

import Model
import JsonModel (FullTreatmentPlan(..))
import Utils

type API =
         "my"
      :>      ("full"
            :> Get '[JSON] [FullTreatmentPlan]
          :<|> "full" :> "formatted"
            :> Get '[PlainText] String
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
        :<|> myFullTreatmentF
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
        
    myFullTreatmentF = do      
      rows <- map entityVal <$> myTreatmentPlanRows
      meds <- db $ selectList [] []
      
      let names = map (\s -> fromMaybe "Unknown"
                        $ medicamentName . entityVal
                        <$> find (\m -> entityKey m
                                   == treatmentPlanRowMedicamentId s)
                            meds)
                  rows
      let quantities = map (show . treatmentPlanRowQuantity) rows
      let times = map (formatTime defaultTimeLocale "%d.%m.%Y (%H:%M)"
                       . treatmentPlanRowAt) rows
      return $ unlines $ zipWith3
        (\q n d -> q ++ " of " ++ n ++ " at " ++ d)
        quantities names times
        
    addTreatmentPlan =
      myDevice >>= db . insert . TreatmentPlan
      
    addTreatmentPlanRow r = do
      let mid = treatmentPlanRowMedicamentId r          
      nex <- fmap isNothing $ db $ selectFirst
        [DeviceStorageMedicamentId ==. mid] []
      when nex $
        myDevice
        >>= db . insert . DeviceStorage 0 mid
        >> return ()
        
      db $ insert r
    
    
    deleteTreatmentPlan = db . delete
    
    deleteTreatmentPlanRow = db . delete
    
    updateTreatmentPlan pid plan =
      db $ replace pid plan
      
    updateTreatmentPlanRow rid row =
      db $ replace rid row
