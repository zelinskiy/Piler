{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module App (startApp, app) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Control.Concurrent

import Server
import Cli
import Types
import Utils

app :: Configuration -> Application
app c = serve (Proxy :: Proxy API) (server c)

startApp :: IO ()
startApp = do
  c0 <- getConfig
  jwt <- getJWT c0

  let c = c0 { token = jwt }
      
  forkIO $ do
    putStrLn "Started Server"
    run (clientPort c) (app c)
  
  putStrLn "Started CLI"
  cliLoop c
