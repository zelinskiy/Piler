module Types.Medicament where

import Data.Maybe(Maybe(Just))
import Data.Argonaut (class EncodeJson, class DecodeJson)
import Data.Generic (class Generic)
import Data.Argonaut.Generic.Aeson (encodeJson, decodeJson) as A

import Data.Newtype (class Newtype)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Symbol (SProxy(..))
import Data.Function((<<<))

newtype Medicament = Medicament
                { id :: Int
                , name :: String
                , diameter :: Int
                , height :: Int
                , description :: Maybe String } 

derive instance genericMedicament :: Generic Medicament

instance decodeJsonMedicament :: DecodeJson Medicament where
  decodeJson = A.decodeJson

instance encodeJsonMedicament :: EncodeJson Medicament where
  encodeJson = A.encodeJson

defaultMedicament :: Medicament
defaultMedicament = Medicament
                { id: 0
                , name: "New medicament"
                , diameter: 1
                , height: 1
                , description: Just "Some description" } 


derive instance newtypeMedicament :: Newtype Medicament _

id :: Lens' Medicament Int
id = _Newtype <<< prop (SProxy :: SProxy "id")

name :: Lens' Medicament String
name = _Newtype <<< prop (SProxy :: SProxy "name")

diameter :: Lens' Medicament Int
diameter = _Newtype <<< prop (SProxy :: SProxy "diameter")

height :: Lens' Medicament Int
height = _Newtype <<< prop (SProxy :: SProxy "height")

description :: Lens' Medicament (Maybe String)
description = _Newtype <<< prop (SProxy :: SProxy "description")
