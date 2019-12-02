{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, TypeFamilies, DataKinds,
  TypeOperators #-}
-- import           Prelude ()
-- import           Prelude.Compat

import qualified Control.Concurrent               as C
import           Control.Concurrent.MVar
import           Control.Exception                (bracket)
import           Control.Lens              hiding (Context)
import           Data.Aeson
import           Data.Aeson.Lens
import qualified Data.HashMap.Strict              as HM
import           Data.Text                        (Text, unpack)
import           GHC.Generics
import           Network.HTTP.Client       hiding (Proxy)
import           Network.HTTP.Types
import           Network.Wai
import qualified Network.Wai.Handler.Warp         as Warp

import           Servant
import           Servant.Client
import           Servant.Server
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal (serverDoesntSatisfy)

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.Matcher

import           CompilationService.API (CompileAll, CompilationServer(..), ContractSource(..), File(..))
import           CompilationService.App (app, newProjectChannel)
import Control.Concurrent (threadDelay)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import qualified Data.Text as T (unpack, length)
import System.Process (readProcessWithExitCode)
import GHC.IO.Exception (ExitCode(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (setCurrentDirectory)

spec :: Spec
spec =
  compileSpec

main :: IO ()
main =
  withSystemTempDirectory "hokey-pokey.tests" $ \dir -> do
    setCurrentDirectory dir
    hspec spec

withUserApp :: IO () -> IO ()
withUserApp action = do
  projectChannel <- newProjectChannel
  -- we can spin up a server in another thread and kill that thread when done
  -- in an exception-safe way
  bracket (liftIO $ C.forkIO $ Warp.run 8888 $ logStdoutDev $
      app $ CompilationServer projectChannel)
    C.killThread
    (const action)

cabalFile = File
  { filename = "test.cabal"
  , contents = "\
     \cabal-version:  1.12\n\
     \name:           test\n\
     \version:        0.1.0.0\n\
     \build-type:     Simple\n\
     \executable test\n\
     \  main-is: Main.hs\n\
     \  build-depends:\n\
     \      base >=4.7 && <5\n\
     \  default-language: Haskell2010\n\
     \"
  }

mainFile = File
  { filename = "Main.hs"
  , contents = "\
     \module Main where\n\n\
     \main = putStrLn \"Hello\"\n\
     \"
  }

compileSpec :: Spec
compileSpec =
  -- `around` will start our Server before the tests and turn it off after
  around_ withUserApp $ do
    -- create a test client function
    let compileAll = client (Proxy :: Proxy CompileAll)
    -- create a servant-client ClientEnv
    baseUrl <- runIO $ parseBaseUrl "http://localhost:8888"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv = mkClientEnv manager baseUrl

    -- testing scenarios start here
    describe "POST /compile/link" $
      it "should compile to all.js" $ do
        result <- runClientM (compileAll ContractSource { files = [cabalFile, mainFile] }) clientEnv
        case result of
          Right [File { filename = "all.js", contents } ] -> do
            -- Make sure it runs
            (exitCode, out, err) <- readProcessWithExitCode "node" [] $ T.unpack contents
            exitCode `shouldBe` ExitSuccess
            out `shouldBe` "Hello\n"
            err `shouldBe` ""
          Left _ -> expectationFailure $ show result
