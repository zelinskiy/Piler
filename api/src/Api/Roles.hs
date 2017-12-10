{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Api.Roles where

import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Servant
import Servant.Server.Internal.RoutingApplication
import Servant.Server.Internal.Router
import Data.Typeable
import GHC.TypeLits

import Model

data WithRole deriving Typeable

newtype GetUserCtx = GetUserCtx { unGetUserCtx :: Maybe User }

instance ( HasServer api ctx
         , HasContextEntry ctx GetUserCtx)
  => HasServer (WithRole :> api) ctx where
  type ServerT (WithRole :> api) m = UserStatus -> ServerT api m
  route :: Proxy (WithRole :> api)
        -> Context ctx
        -> Delayed env (Server (WithRole :> api))
        -> Router env 
  route Proxy ctx subserver =
    route (Proxy :: Proxy api) ctx (subserver `addAuthCheck` authCheck)
    where
      authCheck = withRequest $ \req ->
                    liftIO $ return $ userStatus user
      Just user = unGetUserCtx $ getContextEntry ctx
      
