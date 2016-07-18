{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Web.TodoMVC.Frontend.ReactFlux.TodoRouter where



import           Control.Applicative                      ((<|>))
import           Control.Monad.IO.Class                   (liftIO)
import           Data.Typeable                            (Typeable)
import           GHC.Generics                             ()
import           React.Flux
import           React.Flux.Router.WebRoutes              (initRouter)
import           Web.Routes
import           Web.TodoMVC.Backend.Pure.Todo.Types      (TodoParam (..),
                                                           TodoState (..))
import           Web.TodoMVC.Frontend.ReactFlux.TodoStore



data TodoRoute
  = RouteAll
  | RouteActive
  | RouteCompleted
  deriving (Show, Generic, Typeable)

instance PathInfo TodoRoute where
  toPathSegments route =
    case route of
      RouteAll       -> pure "#"
      RouteActive    -> pure "#active"
      RouteCompleted -> pure "#completed"
  fromPathSegments =
        RouteCompleted <$ segment "#completed"
    <|> RouteActive    <$ segment "#active"
    <|> pure RouteAll



-- | This is messy I know, but it's very late.
--
initTodoRouter :: IO ()
initTodoRouter = do
  initRouter (Just go) go
  where
  go = \segments -> do
    either (const routeError) id $ runSite "" site segments
    executeAction $ SomeStoreAction todoStore TodosList
    where
      site = mkSitePI $ runRouteT $ routerAlterStore
      routerAlterStore action = do
        case action of
          RouteAll       -> liftIO $ alterStore todoStore $ Internal_SetTodoParam (TodoParam Nothing Nothing Nothing)
          RouteActive    -> liftIO $ alterStore todoStore $ Internal_SetTodoParam (TodoParam Nothing Nothing (Just Active))
          RouteCompleted -> liftIO $ alterStore todoStore $ Internal_SetTodoParam (TodoParam Nothing Nothing (Just Completed))

      routeError = alterStore todoStore $ Internal_SetHash "#"
