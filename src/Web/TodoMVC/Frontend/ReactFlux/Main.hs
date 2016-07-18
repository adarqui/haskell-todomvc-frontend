module Web.TodoMVC.Frontend.ReactFlux.Main (
  reactFluxMain
) where



import           Data.Monoid                              ((<>))
import           React.Flux
import           React.Flux.Router.WebRoutes              (initRouter)
import           Web.TodoMVC.Frontend.ReactFlux.TodoStore
import           Web.TodoMVC.Frontend.ReactFlux.TodoView



-- reactRender :: Typeable props => String -> ReactView props -> props -> IO ()
--
-- data SomeStoreAction = forall storeData. (StoreData storeData, NFData (StoreAction storeData))
--    => SomeStoreAction (ReactStore storeData) (StoreAction storeData)
--
-- executeAction :: SomeStoreAction -> IO ()
--
-- todoApp :: ReactView ()
--
reactFluxMain :: IO ()
reactFluxMain = do
  initAjax
  reactRender
    "todoapp" -- String
    todoApp   -- ReactView props
              -- ReactView ()
    ()        -- props
              -- ()
              --

  initRouter $ \segments -> do
    putStrLn $ "segments: " <> show segments

  executeAction $
    SomeStoreAction todoStore TodosList -- SomeStoreAction
