{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module Cli where

import Data.String.Conversions
import Network.HTTP.Simple hiding (Proxy)
import System.Hardware.Serialport
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as C8

import Types (Configuration(..))
import Utils

cliLoop :: Configuration -> IO ()
cliLoop c = do
  cmds <- words <$> getLine
  case cmds of
    ["refill", medname, quantity] ->
      let route = root
            <> "/refill/" <> cs medname
            <> "/"        <> cs quantity 
      in get jwt route
      >>= C8.putStrLn . getResponseBody

    ["pullout", medname, quantity] ->
      let route = root <> "/pullout"
            <> "/" <> cs medname
            <> "/" <> cs quantity 
      in get jwt route
      >>= C8.putStrLn . getResponseBody
      
    ["cmd", cmd] ->
      send h (cs cmd) >> return ()
    
    ["status"] ->
      get jwt (root <> "/status")
      >>= C8.putStrLn . getResponseBody

    ["id"] ->
      get jwt (root <> "/id")
      >>= C8.putStrLn . getResponseBody

    ["rebind", "ip", ip] ->
      putStrLn "Not implemented"
    
    ["q"] -> do
      closeSerial h
      putStrLn "goodbye!"
      exitWith ExitSuccess

    ["help"] -> putStrLn help
    ["h"] -> putStrLn help

    ["help", cmd] -> putStrLn $ helpCmd cmd
    
    _ -> putStrLn (unwords cmds)
    
  cliLoop c
  where
    jwt = token c
    h = devicePort c
    root = serverUrl c <> "/private/device/my"
    help = unlines
      [ "refill medname n         - refill storage"
      , "pullout medname n        - empty storage"
      , "cmd [{F,B,D,S,L,N,W,M}]  - raw command"
      , "status                   - get device status"
      , "id                       - get device id"
      , "rebind ip IP             - rebind device ip"
      , "q                        - exit"
      , "{help|h}                 - show this help"
      , "help [{F,B,D,S,L,N,W,M}] - cmds meaning" ]
    helpCmd "F" = "Pull caret [F]orward"
    helpCmd "B" = "Pull caret [B]ackward"
    helpCmd "D" = "[D]ispence pill = FB"
    helpCmd "S" = "[S]ound alarm"
    helpCmd "L" = "[L]ight alarm"
    helpCmd "N" = "Do [N]othing"
    helpCmd "W" = "Un[M]ute (W = flipped M)"
    helpCmd "M" = "[M]ute sound"

