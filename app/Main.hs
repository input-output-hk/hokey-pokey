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
  c <- newBoundedChan maxPreloadedProjects
  _ <- forkIO $ writeList2Chan c (map (Project . show) [1..])

  run 8080 . app $ CompilationServer c

  return ()


api :: Proxy BasicCompilationAPI
api = Proxy


app :: CompilationServer -> Application
app readData = serve api $ hoistServer api (`runReaderT` readData) compileServer
