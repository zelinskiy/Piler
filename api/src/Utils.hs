{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Utils where

import Database.Persist.Sql
import Servant
import Control.Monad.IO.Class
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Trans.Reader
import Servant.Utils.Enter
import Control.Monad.Trans.Class

import Model

type PublicHandler = ReaderT ConnectionPool Handler
type PrivateHandler = ReaderT (Entity User) PublicHandler

type PublicServer api = ServerT api PublicHandler
type PrivateServer api = ServerT api PrivateHandler

privateToPublicH :: Entity User
                 -> PrivateHandler :~> PublicHandler
privateToPublicH u = NT $ \h -> runReaderT h u

publicToNormalH :: ConnectionPool
                -> PublicHandler :~> Handler
publicToNormalH p = NT $ \h -> runReaderT h p

-- There might be a hack with Type Families or else

db q = lift ask >>= liftIO . runSqlPersistMPool q

db2 q = ask >>= liftIO . runSqlPersistMPool q

hash :: String -> String
hash = BS.unpack . SHA256.hash . BS.pack

enterRole :: Enter
  (Entered PrivateHandler PrivateHandler a)
  PrivateHandler
  PrivateHandler
  a =>
  (UserStatus -> Bool)
  -> Entered PrivateHandler PrivateHandler a
  -> a
enterRole p = enter $ checkRole p

checkRole :: (UserStatus -> Bool)
          -> PrivateHandler :~> PrivateHandler
checkRole p = NT $ \h -> do
  r <- userStatus <$> entityVal <$> ask
  if p r then h else throwError err401

--vertical (componentwise) composition of NT
ver :: f :~> g -> g :~> h -> f :~> h
ver (NT eta) (NT eps) = NT $ eps . eta
    
