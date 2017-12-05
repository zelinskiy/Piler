module TickTack (run) where

import Servant
import Network.Wai
import Database.Persist.Sql

import Model
import Utils

-- Doing some scheduling
run :: ConnectionPool -> IO ()
run pool = do
  putStrLn "Started TickTack Service."

