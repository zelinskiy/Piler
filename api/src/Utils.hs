module Utils where

import Database.Persist.Sql
import Servant
import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as BS

-- TODO:
-- There must be some 2-orer polymorphic hack
-- to prevent `a` from binding on 2 argument
exPool :: ConnectionPool
       -> SqlPersistM a
       -> Handler a
exPool pool = liftIO . flip runSqlPersistMPool pool

hash :: String -> String
hash = BS.unpack . SHA256.hash . BS.pack
