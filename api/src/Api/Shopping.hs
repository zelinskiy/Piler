{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module Api.Shopping
    ( API
    , server
    ) where

import Database.Persist.Sqlite
import Control.Monad.IO.Class
import Servant

import Model
import Utils

type API =
     "list" :> ("all"
            :> Get '[JSON] [Entity ShoppingList]
           :<|> "get"
            :> Capture "id" (Key ShoppingList) 
            :> Get '[JSON] (Maybe ShoppingList)
           :<|> "new"
             :> Capture "name" String
             :> Get '[JSON] (Key ShoppingList)
           :<|> "update"
             :> Capture "id" (Key ShoppingList) 
             :> ReqBody '[JSON] ShoppingList
             :> Post '[JSON] ()
           :<|> "delete"
             :> Capture "id" (Key ShoppingList)
             :> Delete '[JSON] ())
  :<|> "row" :> ("in"
            :> Capture "lid" (Key ShoppingList)
            :> Get '[JSON] [Entity ShoppingListRow]
          :<|> "get"
            :> Capture "lid" (Key ShoppingList) 
            :> Capture "mid" (Key Medicament) 
            :> Get '[JSON] (Maybe ShoppingListRow)
           :<|> "new"
             :> ReqBody '[JSON] ShoppingListRow
             :> Post '[JSON] (Key ShoppingListRow)
           :<|> "update"
             :> Capture "lid" (Key ShoppingList) 
             :> Capture "mid" (Key Medicament)
             :> ReqBody '[JSON] ShoppingListRow
             :> Post '[JSON] ()
           :<|> "delete"
             :> Capture "lid" (Key ShoppingList) 
             :> Capture "mid" (Key Medicament)
             :> Delete '[JSON] ())

server :: ConnectionPool -> Entity User -> Server API
server p me =
  (allLists
   :<|> getList
   :<|> newList
   :<|> updateList
   :<|> deleteList)
  :<|>
  (allRows
    :<|> getRow
    :<|> newRow
    :<|> updateRow
    :<|> deleteRow)
  where
    allLists = exPool p $
      selectList [ShoppingListUserId ==. entityKey me] []

    getList =
      exPool p . get
    newList name = exPool p $ insert $
      ShoppingList name (entityKey me)
    updateList k =
      exPool p . replace k
    deleteList =
      exPool p . delete

    allRows lid = exPool p $
      selectList [ShoppingListRowShoppingListId ==. lid] []

    getRow lid mid =
      exPool p $ get (ShoppingListRowKey lid mid)
    newRow =
      exPool p . insert
    updateRow lid mid =
      exPool p . replace (ShoppingListRowKey lid mid)
    deleteRow lid mid =
      exPool p $ delete (ShoppingListRowKey lid mid)

