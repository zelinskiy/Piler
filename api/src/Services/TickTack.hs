{-# LANGUAGE OverloadedStrings #-}
module Services.TickTack (run) where

import Servant
import Network.Wai
import Database.Persist.Sql hiding (get)
import Control.Concurrent
import Control.Monad.Logger
import Control.Monad.Trans

import Model
import Utils

-- Doing some scheduling
run :: (MonadIO m, MonadLogger m)
    => ConnectionPool -> m ()
run pool =  do
  logInfoN "Started TickTack Service."
  --go pool

go pool = do
  logInfoN "Tick"
  sleep
  logInfoN "Tack"
  sleep
  go pool
  where
    sleep = liftIO $ threadDelay 1000000
