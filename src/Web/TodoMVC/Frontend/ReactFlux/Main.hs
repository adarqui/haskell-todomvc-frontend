module Web.TodoMVC.Frontend.ReactFlux.Main (
  reactFluxMain
) where



import           React.Flux
import           Web.TodoMVC.Frontend.ReactFlux.TodoView



-- reactRender :: Typeable props => String -> ReactView props -> props -> IO ()
-- todoApp :: ReactView ()
--
reactFluxMain :: IO ()
reactFluxMain = do
  reactRender
    "todoapp" -- String
    todoApp   -- ReactView props
              -- ReactView ()
    ()        -- props
              -- ()
