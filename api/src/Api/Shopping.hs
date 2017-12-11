module Api.Shopping
    ( API
    , server
    ) where

import Database.Persist.Sqlite
import Servant
import Control.Monad.Trans.Reader

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

     
server :: PrivateServer API
server = enterRole (>= Silver) $
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
    
    allLists = ask >>= \me -> db $
      selectList [ShoppingListUserId ==. entityKey me] []
    
    getList =
      db . get
    newList name = do
      me <- ask
      db $ insert $ ShoppingList name (entityKey me)
    updateList k =
      db . replace k
    deleteList =
      db . delete

    allRows lid =
      db $ selectList
        [ShoppingListRowShoppingListId ==. lid] []

    getRow lid mid =
      db $ get $ ShoppingListRowKey lid mid
    newRow =
      db . insert
    updateRow lid mid =
      db . replace (ShoppingListRowKey lid mid)
    deleteRow lid mid =
      db $ delete $ ShoppingListRowKey lid mid

