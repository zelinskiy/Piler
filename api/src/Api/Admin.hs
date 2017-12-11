module Api.Admin (API, server) where

import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Servant
import Control.Monad.Trans.Reader
import System.Random

import Model
import Utils

type API =
         "seize"
      :> "the"
      :> "means"
      :> Get '[JSON] String
    :<|> "keys"
      :> "generate"
      :> Capture "purpose" SecretKeyPurpose
      :> Get '[JSON] SecretKey


server :: PrivateServer API
server = seizeTheMeans
         :<|> enterRole (== Admin) generateKey 
  where
    seizeTheMeans = do
      uid <- entityKey <$> ask
      db $ update uid [UserStatus =. Admin]
      return "Workers of the world, unite!"
    generateKey purpose = do
      key <- return . take 10
             . randomRs ('A','Z') =<< liftIO newStdGen

      let k = SecretKey { secretKeyValue = key
                        , secretKeyPurpose = purpose }
      db $ insert k
      return k
