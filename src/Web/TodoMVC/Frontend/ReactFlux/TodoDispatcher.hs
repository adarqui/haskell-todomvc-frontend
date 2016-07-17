module Web.TodoMVC.Frontend.ReactFlux.TodoDispatcher (
  dispatchTodo
) where



import           React.Flux
import           Web.TodoMVC.Frontend.ReactFlux.TodoStore



-- data SomeStoreAction = forall storeData. (StoreData storeData, NFData (StoreAction storeData))
--    => SomeStoreAction (ReactStore storeData) (StoreAction storeData)
-- *
-- instance NFData SomeStoreAction where
--    rnf (SomeStoreAction _ action) = action `deepseq` ()
--
dispatchTodo :: TodoAction -> [SomeStoreAction]
dispatchTodo a = [SomeStoreAction todoStore a]
