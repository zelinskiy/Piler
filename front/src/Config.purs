module Config where

type Config = { serverRoot :: String }

config :: Config
config = { serverRoot: serverRoot }

serverRoot :: String
serverRoot = "http://localhost:8080/"

