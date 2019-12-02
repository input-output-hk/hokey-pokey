module CompilationService.App
 ( newProjectChannel
 , app
 )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.BoundedChan (BoundedChan, newBoundedChan, writeChan)

import Data.Foldable (forM_)
import Data.Functor (void)

import Network.Wai (Application)

import Servant (serve)

import CompilationService.API
       (api, CompilationServer(..), Project(..),
        compileServer, makeProject, maxPreloadedProjects)

newProjectChannel :: IO (BoundedChan Project)
newProjectChannel = do
  c <- newBoundedChan maxPreloadedProjects
  void . forkIO . forM_ [1 :: Int ..] $ \n ->
    writeChan c =<< makeProject (show n)
  return c

app :: (String -> CompilationServer) -> Application
app readData = serve api (compileServer readData)

