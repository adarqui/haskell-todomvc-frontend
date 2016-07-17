{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module Web.TodoMVC.Frontend.ReactFlux.TodoStore where



import           Control.DeepSeq
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import           Data.Typeable                       (Typeable)
import           GHC.Generics                        (Generic)
import           React.Flux
import           Web.TodoMVC.Backend.Pure.Todo.Types (TodoId, TodoResponse (..))



data TodoStore = TodoStore {
  tsTodos       :: Map TodoId TodoResponse,
  tsCurrentTodo :: Maybe TodoId
} deriving (Show, Typeable)



data TodoAction
  = TodoCreate          Text
  | TodoDelete          TodoId
  | TodoEdit            TodoId
  | UpdateText          TodoId Text
  | ToggleAllComplete
  | TodoSetComplete     TodoId Bool
  | ClearCompletedTodos
  deriving (Show, Typeable, Generic, NFData)



-- transform :: StoreAction storeData -> storeData -> IO storeData
--
instance StoreData TodoStore where
  type StoreAction TodoStore = TodoAction
  transform action st@TodoStore{..} = do

    -- Care is taken here to leave the Haskell object for the pair (Int, Todo) unchanged if the todo
    -- itself is unchanged.  This allows React to avoid re-rendering the todo when it does not change.
    -- For more, see the "Performance" section of the React.Flux haddocks.
    st' <- case action of
      (TodoCreate txt) -> do -- st { tsTodos = (maximum (map fst todos) + 1, Todo txt False False) : todos }
                          pure st
      (TodoDelete i)   -> do -- TodoStore (filter ((/=i) . fst) todos) currentTodo
                          pure st
      (TodoEdit i)     -> do -- TodoStore todos (Just i)
                          pure st
      (UpdateText newIdx newTxt) -> do
                                    pure st
--          let f (idx, todo) | idx == newIdx = (idx, todo { todoText = newTxt, todoIsEditing = False })
--              f p = p
--           in map f todos
      ToggleAllComplete          -> do -- [ (idx, Todo txt True False) | (idx, Todo txt _ _) <- todos ]
                                    pure st
      TodoSetComplete newIdx newComplete -> do
                                            pure st
--          let f (idx, todo) | idx == newIdx = (idx, todo { todoComplete = newComplete })
--              f p = p
--           in map f todos
      ClearCompletedTodos        -> do
                                    pure st -- filter (not . todoComplete . snd) todos

    pure st'



-- newtype ReactStoreRef storeData = ReactStoreRef JSVal
--
-- data ReactStore storeData = ReactStore {
--    storeRef  :: ReactStoreRef storeData
--  , storeData :: MVar storeData
-- }
--
-- | Create a new store from the initial data.
-- mkStore :: StoreData storeData => storeData -> ReactStore storeData
--
todoStore :: ReactStore TodoStore
todoStore = mkStore $ TodoStore Map.empty Nothing
