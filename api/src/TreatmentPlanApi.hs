{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module TreatmentPlanApi
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
      :> ReqBody '[JSON] TreatmentPlan
      :> Post '[JSON] (Key TreatmentPlan)
    :<|> "delete"
      :> Capture "id" (Key TreatmentPlan)
      :> Delete '[JSON] ()

server :: ConnectionPool -> Entity User -> Server API
server p me =
       myTreatmentPlans
  :<|> addTreatmentPlan
  :<|> deleteTreatmentPlan
  where
    myTreatmentPlans = exPool p $
        selectList [TreatmentPlanUserId ==. entityKey me] []
    addTreatmentPlan = exPool p . insert
    deleteTreatmentPlan pid = exPool p $
        deleteWhere [TreatmentPlanId ==. pid]

