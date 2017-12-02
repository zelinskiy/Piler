{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Api.Device
    ( API
    , server
    ) where

import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Servant
import Data.Either
import Data.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource.Internal

import Model
import Utils

type API =
         "my"
      :> Get '[JSON] [Entity TreatmentPlan]
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
      
      

server :: ConnectionPool -> Entity User -> Server API
server p me =
       myTreatmentPlans
  :<|> (addTreatmentPlan    :<|> addTreatmentPlanRow)
  :<|> (deleteTreatmentPlan :<|> deleteTreatmentPlanRow)
  :<|> (updateTreatmentPlan :<|> updateTreatmentPlanRow)
  where
    myTreatmentPlans = exPool p $
      selectList [TreatmentPlanUserId ==. entityKey me] []
    addTreatmentPlan = exPool p $ insert $
      TreatmentPlan { treatmentPlanUserId = entityKey me }
    addTreatmentPlanRow = exPool p . insert
    deleteTreatmentPlan pid = exPool p $
      deleteWhere [TreatmentPlanId ==. pid]
    deleteTreatmentPlanRow rid = exPool p $
      deleteWhere [TreatmentPlanRowId ==. rid]
    updateTreatmentPlan pid plan =
      exPool p $ replace pid plan
    updateTreatmentPlanRow rid row =
      exPool p $ replace rid row
