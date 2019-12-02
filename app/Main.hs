module Main where

import CompilationService.API (CompilationServer(..))
import CompilationService.App (app, newProjectChannel)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main =
  run 8080 . app . CompilationServer =<< newProjectChannel


