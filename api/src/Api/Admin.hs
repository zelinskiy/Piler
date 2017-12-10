{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module Api.Admin (API, server) where

import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Servant
import Data.Either
import Data.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Logger
import Control.Monad.Trans.Resource.Internal
import GHC.Generics
import Data.Aeson
import Data.Traversable
import Data.Monoid
import Control.Monad
import System.Random
import Data.Char

import Model
import JsonModel(RegisterData(..))
import Utils

type API =
         "keys"
      :> "generate"
      :> Capture "purpose" SecretKeyPurpose
      :> Get '[JSON] SecretKey



server :: ConnectionPool -> Entity User -> Server API
server p me =
       generateKey 
  where
    checkAdmin :: Handler ()
    checkAdmin =
      if userStatus (entityVal me) == Admin
      then return ()
      else throwError err401
    generateKey purpose = do
      checkAdmin
      key <- return . take 10
             . randomRs ('A','Z') =<< liftIO newStdGen

      let k = SecretKey { secretKeyValue = key
                        , secretKeyPurpose = purpose }
      exPool p $ insert k
      return k
