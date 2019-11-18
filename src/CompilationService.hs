{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module CompilationService where

import Data.Serialize
import GHC.Generics
import Servant.API
import Servant.API.ContentTypes
import System.Process
import Control.Concurrent.BoundedChan
import Control.Monad.Reader
import Data.ByteString as BS
import Servant
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.List as L (isSuffixOf, find)
import Data.Maybe
import GHC.IO.Handle as H
import Distribution.Parsec.Parser
import Distribution.Parsec.Field


-- type CompliationAPI = BasicCompiler :<|> IncrementalCompiler

type BasicCompilationAPI

  -- With a set of input files, produce a both unlinked compilation
  -- outputs and the result, linked, file:
  =    "compile" :> "all"
                 :> ReqBody '[OctetStream] ContractSource
                 :> Post    '[OctetStream] [File]

  -- With a set of input files, produce only a linked result output.
  :<|> "compile" :> "link"
                 :> ReqBody '[OctetStream] ContractSource
                 :> Post    '[OctetStream] File


data ContractSource
   = ContractSource
   { files :: [File]
   } deriving (Generic, Serialize)

data File = File
   { filename :: FilePath
   , contents :: ByteString
   } deriving (Generic, Serialize)

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
   }


type Compiler = ReaderT CompilationServer Handler

instance MimeRender OctetStream ContractSource where
  mimeRender _ = fromStrict . encode

instance MimeUnrender OctetStream ContractSource where
  mimeUnrender _ = decode . toStrict

instance MimeRender OctetStream File where
  mimeRender _ = fromStrict . encode

instance MimeUnrender OctetStream File where
  mimeUnrender _ = decode . toStrict

instance MimeRender OctetStream [File] where
  mimeRender _ = fromStrict . encode


compileServer :: ServerT BasicCompilationAPI Compiler
compileServer = handleBasicCompilation
           :<|> fmap linkedResult . handleBasicCompilation


handleBasicCompilation :: ContractSource -> Compiler [File]
handleBasicCompilation source = do
  project <- getProject
  saveContractSource project source
  invokeCompilation project
  getOutputFiles project


projectInfo :: ContractSource -> (String, [String])
projectInfo (ContractSource files) = (show $ Prelude.head executableNames, [])
  where
    Just  cabalFile   = L.find (\f -> ".cabal" `L.isSuffixOf` filename f) files
    Right cabalFields = readFields (contents cabalFile)
    --
    executableNames = catMaybes (Prelude.map executableSection cabalFields)
    --
    executableSection (Section sec [SecArgStr _ name] _) | getName sec == "executable" = Just name
    executableSection _                                                = Nothing


makeProject :: ProjectId -> IO Project
makeProject pid = do
  makeDirectory pid
  copyDefaultProject pid
  let cmd = (shell $ "read && cabal build"){cwd = Just ("./projects/" ++ pid), std_in = CreatePipe, std_out = CreatePipe, std_err = CreatePipe}
  (Just stdin, Just stdout, Just stderr, proc) <- createProcess cmd
  return (Project pid (Process proc stdin stdout stderr))


wake :: Process -> Compiler ()
wake (Process _ stdin _ _) = liftIO $ hPutChar stdin '\n'


makeDirectory :: ProjectId -> IO ()
makeDirectory pid = void $
  readCreateProcess (shell $ "mkdir " ++ show pid){cwd = Just "./projects"} ""


copyDefaultProject :: ProjectId -> IO ()
copyDefaultProject pid = void $
  readCreateProcess (shell $ "cp defaultProject ./projects/" ++ pid) ""


saveContractSource :: Project -> ContractSource -> Compiler ()
saveContractSource (Project pid _) (ContractSource inputs) = mapM_ go inputs
  where
    go (File name contents) = liftIO $ BS.writeFile ("./projects/src/" ++ name) contents


getProject :: Compiler Project
getProject = do -- (projects <$> ask) >>= (liftIO . readChan)
  CompilationServer ps <- ask
  liftIO $ readChan ps


invokeCompilation :: Project -> Compiler String
invokeCompilation (Project _ proc@(Process _ _ _ stderr)) = do
  wake proc
  liftIO (H.hGetContents stderr)


-- TODO: figure out exactly where Cabal puts all of the outputs that we want
-- to return, for both ghc and ghcjs.
getOutputFiles :: Project -> Compiler [File]
getOutputFiles idx = do
  return undefined


-- Might need to put a field in the reader state that deals with which ghc
-- variant we are using.
linkedResult :: [File] -> File
linkedResult = fromJust . L.find (L.isSuffixOf "jsexe" . filename)
