module Api.Auth
    ( authContext
    , Private
    ) where


import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Servant
import Servant.Server.Experimental.Auth
import Network.Wai
import Data.ByteString.Char8(unpack)

import Model
import Utils

type instance AuthServerData (AuthProtect "cookie-auth") = Entity User

type Private = AuthProtect "cookie-auth" 

authContext :: ConnectionPool
            -> AuthHandler Request (Entity User)
authContext pool = mkAuthHandler $ \req ->
  let h = requestHeaders req
      throw401 m = throwError (err401 { errBody = m })
  in case (lookup "email" h, lookup "password" h) of
    (Nothing, Nothing) -> 
      throw401 "Missing password & email"
    (Nothing, Just _) ->
      throw401 "Missing email"
    (Just _, Nothing) ->
      throw401 "Missing password"
    (Just e, Just p) -> do
      mbUser <- liftIO $ flip runSqlPersistMPool pool $
        selectFirst [ UserEmail    ==. unpack e
                    , UserPassword ==. hash (unpack p) ] []
                                        
      case mbUser of
        Just u -> return u
        Nothing ->
          throwError (err403 { errBody = "Can't find user" })




