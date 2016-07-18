{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Web.TodoMVC.Frontend.ReactFlux.TodoStore where



import           Control.DeepSeq                     (NFData)
import           Control.Monad                       (forM_)
import           Control.Monad.IO.Class              (liftIO)
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Monoid                         ((<>))
import           Data.Text                           (Text)
import qualified Data.Text                           as Text
import           Data.Typeable                       (Typeable)
import           GHC.Generics                        (Generic)
import           React.Flux
import           Safe                                (headMay)
import           Web.TodoMVC.Backend.Pure.Todo.Types (TodoId, TodoParam (..),
                                                      TodoRequest (..),
                                                      TodoResponse (..),
                                                      TodoResponses,
                                                      TodoState (..),
                                                      defaultTodoParam,
                                                      flipTodoState,
                                                      todoResponseToRequest)



data TodoStore = TodoStore {
  tsTodos :: !(Map TodoId TodoResponse),
  tsParam :: !TodoParam
} deriving (Show, Generic, Typeable, NFData)



data TodoAction
  = TodosList
  | TodoCreate             Text
  | TodoDelete             TodoId
  | TodoEdit               TodoId
  | TodoUpdate             TodoId TodoRequest
  | TodosToggleAllComplete
  | TodosClearCompleted
  | Internal_SetTodos      TodoResponses
  | Internal_AddTodo       (Maybe TodoResponse)
  | Internal_DeleteTodo    TodoId
  | Internal_AjaxError     Text
  deriving (Show, Typeable, Generic, NFData)



-- transform :: StoreAction storeData -> storeData -> IO storeData
--
-- jsonAjax :: (ToJSON body, FromJSON response)
--          => Text -- ^ the method
--          -> Text -- ^ the URI
--          -> [(Text, Text)] -- ^ the headers.  In addition to these headers, 'jsonAjax' adds two headers:
--                            -- @Content-Type: application/json@ and @Accept: application/json@.
--          -> body -- ^ the body
--          -> (Either (Int, Text) response -> IO [SomeStoreAction])
--
instance StoreData TodoStore where
  type StoreAction TodoStore = TodoAction
  transform action st@TodoStore{..} = do

    liftIO $ putStrLn $ "transform action: " <> show action

    -- Care is taken here to leave the Haskell object for the pair (Int, Todo) unchanged if the todo
    -- itself is unchanged.  This allows React to avoid re-rendering the todo when it does not change.
    -- For more, see the "Performance" section of the React.Flux haddocks.
    st' <- case action of
      TodosList                   -> action_todos_list
      TodoCreate title            -> action_todo_create title
      TodoDelete todo_id          -> action_todo_delete todo_id
      TodoEdit todo_id            -> action_todo_edit todo_id
      TodoUpdate todo_id req      -> action_todo_update todo_id req
      TodosToggleAllComplete      -> action_todos_toggle_all_complete
      TodosClearCompleted         -> action_todos_clear_completed

      Internal_SetTodos todos     -> internal_set_todos todos
      Internal_AddTodo m_todo     -> internal_add_todo m_todo
      Internal_DeleteTodo todo_id -> internal_delete_todo todo_id
      Internal_AjaxError _        -> pure st

    pure st'

    where

    action_todos_list = do
      jsonAjax "GET" "/todos" [] () $ \case
        Left (_, msg) -> pure [SomeStoreAction todoStore $ Internal_AjaxError msg]
        Right todos   -> pure [SomeStoreAction todoStore $ Internal_SetTodos todos]
      pure st

    action_todo_create title = do
      jsonAjax "POST" "/todos" [] (TodoRequest title Active) $ \case
        Left (_, msg) -> pure [SomeStoreAction todoStore $ Internal_AjaxError msg]
        Right m_todo  -> pure [SomeStoreAction todoStore $ Internal_AddTodo m_todo]
      pure st

    action_todo_delete todo_id = do
      jsonAjax "DELETE" ("/todos/" <> (Text.pack $ show todo_id)) [] () $ \case
        Left (_, msg)         -> pure [SomeStoreAction todoStore $ Internal_AjaxError msg]
        Right Nothing         -> pure [SomeStoreAction todoStore $ Internal_AjaxError "Unable to parse JSON"]
        Right (Just todo_id') -> pure [SomeStoreAction todoStore $ Internal_DeleteTodo todo_id']
      pure st

    action_todo_edit todo_id = do
      let m_req = Map.lookup todo_id tsTodos
      case m_req of
        Nothing   -> pure st
        Just resp -> action_todo_update todo_id ((todoResponseToRequest resp){_todoRequestState = Editing})

    action_todo_update todo_id req = do
      jsonAjax "PUT" ("/todos/" <> (Text.pack $ show todo_id)) [] req  $ \case
        Left (_, msg) -> pure [SomeStoreAction todoStore $ Internal_AjaxError msg]
        Right m_todo  -> pure [SomeStoreAction todoStore $ Internal_AddTodo m_todo]
      pure st

    action_todos_toggle_all_complete = do
      let m_first_todo = headMay $ Map.toList tsTodos
      case m_first_todo of
        Nothing              -> pure st
        Just (_, first_todo) -> do
          let toggle = flipTodoState (_todoResponseState first_todo)
          forM_ (Map.toList tsTodos) $ \(todo_id, resp@TodoResponse{..}) -> do
            action_todo_update todo_id $ (todoResponseToRequest resp){_todoRequestState = toggle}
          pure st

    action_todos_clear_completed = do
      let completed_todos = filter ((==) Completed . _todoResponseState) $ Map.elems tsTodos
      forM_ completed_todos $ \TodoResponse{..} -> do
        action_todo_delete _todoResponseId
      pure st

    internal_set_todos todos = pure $ st{tsTodos = Map.fromList $ zip (map _todoResponseId todos) todos}

    internal_add_todo m_todo = do
      case m_todo of
        Nothing                    -> pure st
        Just todo@TodoResponse{..} -> pure $ st{tsTodos = Map.insert _todoResponseId todo tsTodos}

    internal_delete_todo todo_id = pure $ st{tsTodos = Map.delete todo_id tsTodos}



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
todoStore = mkStore $ TodoStore Map.empty defaultTodoParam
