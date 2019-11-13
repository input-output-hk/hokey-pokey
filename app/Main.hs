module Main where

import CompilationService
import Network.Wai.Handler.Warp
import Control.Concurrent.BoundedChan
import Control.Concurrent (forkIO)
import Data.Proxy
import Servant
import Control.Monad.Reader


main :: IO ()
main = do
  run 8080 . app . CompilationServer =<< newProjectChannel


newProjectChannel :: IO (BoundedChan Project)
newProjectChannel = do
  c <- newBoundedChan maxPreloadedProjects
  void . forkIO $ do
    ps <- mapM (makeProject . show) [1..]
    writeList2Chan c ps
  return c


api :: Proxy BasicCompilationAPI
api = Proxy


app :: CompilationServer -> Application
app readData = serve api $ hoistServer api (`runReaderT` readData) compileServer
