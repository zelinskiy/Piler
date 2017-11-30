module Utils where

import Database.Persist.Sql
import Servant
import Control.Monad.IO.Class

-- TODO:
-- There must be some 2-orer polymorphic hack
-- to prevent `a` from binding on 2 argument
exPool :: ConnectionPool
       -> SqlPersistM a
       -> Handler a
exPool pool = liftIO . flip runSqlPersistMPool pool
