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
      :> Get '[JSON] String


server :: PrivateServer API
server = seizeTheMeans
         :<|> enterRole (== Admin) generateKey 
  where
    seizeTheMeans = do
      me <- ask
      let s' = if userStatus (entityVal me) == Admin
               then Normal else Admin
      db $ update (entityKey me) [UserStatus =. s']
      return "Workers of the world, unite!"
    generateKey purpose = do
      key <- return . take 10
             . randomRs ('A','Z') =<< liftIO newStdGen

      let k = SecretKey { secretKeyValue = key
                        , secretKeyPurpose = purpose }
      db $ insert k
      return key
