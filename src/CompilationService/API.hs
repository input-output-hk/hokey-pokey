{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module CompilationService.API where

import Control.Concurrent
       (killThread, takeMVar, putMVar, forkIO, newEmptyMVar, MVar)
import Control.Concurrent.BoundedChan (readChan, BoundedChan)
import Control.DeepSeq (rnf)
import Control.Exception (throwIO, try, mask, SomeException)
import qualified Control.Exception as C
       (evaluate, handle, onException)
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT(..))

import Data.Aeson (ToJSON, FromJSON)
import Data.ByteString as BS
import Data.List as L (isSuffixOf, find)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Proxy (Proxy(..))
import Data.Swagger (ToSchema)
import Data.Text (Text)
import qualified Data.Text as T (unpack)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)

import Distribution.Parsec.Parser (readFields)
import Distribution.Parsec.Field (Field(..), SectionArg(..), getName)
import Distribution.Simple.Utils (copyDirectoryRecursive)
import Distribution.Verbosity (normal)

import Foreign.C (ePIPE, Errno(..))

import GHC.Generics (Generic)
import GHC.IO.Exception (IOErrorType(..), IOException(..))

import Servant
       (errBody, err400, err501, Server, Handler, ReqBody, Post, JSON,
        (:<|>)(..), (:>))

import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode)
import System.FilePath ((<.>), (</>))
import System.IO (hClose, hPutChar, Handle)
import qualified System.IO as H (hGetContents)
import System.Process
       (waitForProcess, createProcess, shell, ProcessHandle, cwd, std_in,
        std_out, std_err, StdStream(..))

import Paths_hokey_pokey (getDataDir)

-- type CompliationAPI = BasicCompiler :<|> IncrementalCompiler

-- With a set of input files, produce a both unlinked compilation
-- outputs and the result, linked, file:
type CompileAll =
  "compile" :> "all"
    :> ReqBody '[JSON] ContractSource
    :> Post    '[JSON] [File]

-- With a set of input files, produce only a linked result output.
type CompileLink =
  "compile" :> "link"
    :> ReqBody '[JSON] ContractSource
    :> Post    '[JSON] File

type BasicCompilationAPI
  = CompileAll :<|> CompileLink

newtype ContractSource
   = ContractSource
   { files :: [File]
   } deriving (Generic)

instance ToJSON     ContractSource
instance FromJSON   ContractSource
instance ToSchema   ContractSource

data File = File
   { filename :: FilePath
   , contents :: Text
   } deriving (Show, Eq, Generic)

instance ToJSON     File
instance FromJSON   File
instance ToSchema   File

type ProjectId = String

data Project
   = Project
   { idx :: ProjectId
   , process :: Process
   }


data Process
   = Process
   { handle :: ProcessHandle
   , stdin  :: Handle
   , stdout :: Handle
   , stderr :: Handle
   }


-- Use a channel here over a TVar so once we have infrastructure in place to
-- pre-warm ghcjs processes, we can simply switch it over by storing the
-- `System.Process.ProcessHandle` in `Project`.

-- Copying around project layouts might still be some overhead, so we might
-- as well do it in advance, even if it's not as much as warming the compiler.
maxPreloadedProjects :: Int
maxPreloadedProjects = 10

data CompilationServer
   = CompilationServer
   { projects :: BoundedChan Project
   , execFile :: String
   }

type Compiler = ReaderT CompilationServer Handler

compileServer :: (String -> CompilationServer) -> Server BasicCompilationAPI
compileServer reader'
  =    handleBasicCompilation reader'
  :<|> const (throwError err501)


handleBasicCompilation :: (String -> CompilationServer) -> ContractSource -> Handler [File]
handleBasicCompilation reader' source = do
  (contractExecutable, _) <- projectInfo source
  runReaderT (handleBasicCompilation' source) (reader' contractExecutable)


handleBasicCompilation' :: ContractSource -> Compiler [File]
handleBasicCompilation' source = do
  project <- getProject
  saveContractSource project source
  (e, out, err) <- invokeCompilation project
  liftIO $ Prelude.putStrLn out
  liftIO $ Prelude.putStrLn err
  liftIO $ Prelude.putStrLn $ "Exit " <> show e
  getOutputFiles project


projectInfo :: ContractSource -> Handler (String, [String])
projectInfo (ContractSource files) = do
  --
  cabalFile <-
    case L.find (\f -> ".cabal" `L.isSuffixOf` filename f) files of
      Just f  -> return f
      Nothing -> throwError err400 { errBody = "No .cabal file." }
  --
  cabalFields <-
    case readFields (encodeUtf8 $ contents cabalFile) of
      Left _  -> throwError err400 { errBody = "Unable to read cabal file fields." }
      Right c -> return c

  -- liftIO $ Prelude.putStrLn $ ppShow cabalFields
  let executableNames = mapMaybe executableSection cabalFields
  case decodeUtf8' <$> listToMaybe executableNames of
    Nothing -> throwError err400 { errBody = "No executable found in cabal file." }
    Just (Left _) -> throwError err400 { errBody = "Utf8 error decoding executable name." }
    Just (Right exeName) ->
      return (T.unpack exeName, [])

  where
    executableSection (Section sec [SecArgName _ name] _) | getName sec == "executable" = Just name
    executableSection _                                                = Nothing


makeProject :: ProjectId -> IO Project
makeProject pid = do
  let projectDir = "projects" </> pid
  dataDir <- getDataDir
  createDirectoryIfMissing True projectDir
  copyDirectoryRecursive normal (dataDir </> "defaultProject") projectDir
  let cmd = (shell "read && cabal build --ghcjs all")
        {cwd = Just projectDir, std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  (Just stdin, Just stdout, Just stderr, proc) <- createProcess cmd
  return (Project pid (Process proc stdin stdout stderr))


wake :: Process -> IO ()
wake (Process _ stdin _ _) = hPutChar stdin '\n'


saveContractSource :: Project -> ContractSource -> Compiler ()
saveContractSource (Project pid _) (ContractSource inputs) = mapM_ go inputs
  where
    projectDir = "projects" </> pid
    go (File name contents) = liftIO $ BS.writeFile (projectDir </> name) $ encodeUtf8 contents


getProject :: Compiler Project
getProject = do -- (projects <$> ask) >>= (liftIO . readChan)
  CompilationServer ps _ <- ask
  liftIO $ readChan ps

-- From System.Process (not exposed)
-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important in the cases above because we want to kill the thread
-- that is holding the Handle lock, because when we clean up the process we
-- try to close that handle, which could otherwise deadlock.
--
withForkWait :: IO () -> (IO () -> IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `C.onException` killThread tid

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = C.handle $ \e -> case e of
               IOError { ioe_type  = ResourceVanished
                       , ioe_errno = Just ioe }
                 | Errno ioe == ePIPE -> return ()
               _ -> throwIO e

invokeCompilation :: Project -> Compiler (ExitCode, String, String)
invokeCompilation (Project _ proc@(Process ph inh outh errh)) = liftIO $ do
  -- This function is based on System.Process.readCreateProcessWithExitCode
  err <- H.hGetContents errh
  out <- H.hGetContents outh
  -- fork off threads to start consuming stdout & stderr
  withForkWait (C.evaluate $ rnf out) $ \waitOut ->
   withForkWait (C.evaluate $ rnf err) $ \waitErr -> do

    ignoreSigPipe $ wake proc

    -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
    ignoreSigPipe $ hClose inh

    -- wait on the output
    waitOut
    waitErr

    hClose outh
    hClose errh

  -- wait on the process
  ex <- liftIO $ waitForProcess ph
  return (ex, out, err)

-- TODO: figure out exactly where Cabal puts all of the outputs that we want
-- to return, for both ghc and ghcjs.
getOutputFiles :: Project -> Compiler [File]
getOutputFiles (Project pid _proc) = do
  let projectDir = "projects" </> pid
  CompilationServer {execFile} <- ask
  liftIO $ Prelude.putStrLn $ "hello " <> execFile
  js <- liftIO $ BS.readFile (projectDir </> "dist-newstyle/build/x86_64-linux/ghcjs-8.6.0.1/test-0.1.0.0/x"
    </> execFile </> "build"
    </> execFile </> execFile <.> "jsexe" </> "all.js")
  case decodeUtf8' js of
    Left _err -> error "GHCJS output invalid utf8"
    Right jsText -> return [File "all.js" jsText]

api :: Proxy BasicCompilationAPI
api = Proxy
