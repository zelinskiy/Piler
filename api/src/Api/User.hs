module Api.User
    ( API
    , PublicAPI
    , server
    , publicServer
    ) where

import Database.Persist.Sqlite
import Servant
import Control.Monad.Trans.Reader

import Model
import JsonModel(RegisterData(..))
import Utils

type API =
         "me"
      :> Get '[JSON] (Entity User)
    :<|> "unregister"
      :> Get '[JSON] ()
    :<|> "updrade"
      :> Capture "purpose" SecretKeyPurpose
      :> Capture "key" String
      :> Get '[JSON] ()
    
type PublicAPI =
         "register"
      :> ReqBody '[JSON] RegisterData
      :> Post '[JSON] (Key User)

server :: PrivateServer API
server =
       getMyself
  :<|> unregister
  :<|> upgrade
  where
    getMyself = ask
    unregister = ask >>= \me -> db $ do      
      deleteCascade (entityKey me)
    upgrade SubscribeSilver k =  do
      mbKey <- db $ selectFirst
        [ SecretKeyValue ==. k
        , SecretKeyPurpose ==. SubscribeSilver ] []
      case mbKey of
        Nothing -> throwError $ err403
          { errBody = "Key not Found" }
        Just key -> ask >>= \me -> db $ do          
          update (entityKey me) [UserStatus =. Silver]
          delete (entityKey key)
      

publicServer :: PublicServer PublicAPI
publicServer = register
  where
    register RegisterData
      { email = e
      , pass = p
      , ip = i} = db2 $ do      
      uid <- insert $ User
        { userEmail = e
        , userPassword = hash p
        , userStatus = Normal }
      insert $ Device i uid
      return uid
      
