{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Api.Treatment (API, server) where

import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Servant
import Data.Either
import Data.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource.Internal

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
server :: ConnectionPool -> Entity User -> Server API
server p me =
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
    myDevice = fromJust
      <$> fmap entityKey <$> selectFirst
        [DeviceUserId ==. entityKey me] []
    
    myTreatmentPlans = exPool p $ do
      did <- myDevice
      selectList [TreatmentPlanDeviceId ==. did] []
      
    myTreatmentPlanRows = do
      plans <- map entityKey <$> myTreatmentPlans
      exPool p $
        selectList [TreatmentPlanRowTreatmentPlanId <-. plans] []
        
    myFullTreatmentPlans = do
      plans <- myTreatmentPlans
      rows <- map entityVal <$> myTreatmentPlanRows
      let pred p r = treatmentPlanRowTreatmentPlanId r
                             == entityKey p
      return $ map (\p -> FullTreatmentPlan
            { treatmentPlan = entityVal p
            , treatmentPlanRows = filter (pred p) rows })
        plans
        
    addTreatmentPlan = exPool p $ do
      did <- myDevice
      insert $ TreatmentPlan did
      
    addTreatmentPlanRow = exPool p . insert
    
    deleteTreatmentPlan = exPool p . delete
    
    deleteTreatmentPlanRow = exPool p . delete
    
    updateTreatmentPlan pid plan =
      exPool p $ replace pid plan
      
    updateTreatmentPlanRow rid row =
      exPool p $ replace rid row
