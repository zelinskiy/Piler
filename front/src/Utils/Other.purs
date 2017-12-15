module Utils.Other where

import Prelude

import Data.String(take, drop, length)
import Data.Either(Either)

import Data.Argonaut.Generic.Argonaut(options)
import Data.Argonaut.Generic.Options(Options(Options))
import Data.Argonaut.Generic.Encode (genericEncodeJson)
import Data.Argonaut.Generic.Decode (genericDecodeJson)
import Data.Argonaut (Json)
import Data.Generic (class Generic)
                      
trimAny :: String -> String
trimAny = drop 1 <<< (\s -> take (length s - 1) s)

genericNewtypeJsonOptions :: Options
genericNewtypeJsonOptions = Options $ case options of
  Options o -> o { encodeSingleConstructors = false
                 , flattenContentsArray = true }

genericDecodeNewtypeJson :: forall a. Generic a 
                            => Json -> Either String a
genericDecodeNewtypeJson =
  genericDecodeJson genericNewtypeJsonOptions

genericEncodeNewtypeJson :: forall a. Generic a
                            => a -> Json
genericEncodeNewtypeJson =
  genericEncodeJson genericNewtypeJsonOptions
