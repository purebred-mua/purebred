module Purebred (
  module Types,
  module UI.Index.Keybindings,
  module UI.Mail.Keybindings,
  Event(..),
  Key(..),
  Modifier(..),
  getDatabasePath,
  defaultConfig,
  defaultColorMap,
  over,
  set,
  (&),
  purebred,
  halt,
  continue) where

import UI.App (theApp, initialState)

import Control.Monad (unless)
import Control.Exception.Base (SomeException(..), IOException, catch)
import Control.Monad (void)
import Options.Applicative hiding (str)
import qualified Options.Applicative.Builder as Builder
import Data.Semigroup ((<>))
import System.Process
       (createProcess, proc, runProcess, waitForProcess, ProcessHandle)
import System.Info (arch, os)
import System.Exit (ExitCode(..))
import System.Environment (getProgName, lookupEnv, getArgs)
import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.Directory (getModificationTime, getCurrentDirectory)
import System.FilePath.Posix ((</>))
import System.Exit (exitWith)
import System.IO (hPrint, stderr, hFlush)
import Data.Maybe (fromMaybe, isJust)

import UI.Index.Keybindings
import UI.Mail.Keybindings
import Storage.Notmuch (getDatabasePath)
import Config.Main (defaultConfig, defaultColorMap)
import Types

-- re-exports for configuration
import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))
import Brick.Main (halt, continue, defaultMain)
import Control.Lens.Lens ((&))
import Control.Lens.Setter (over, set)
import Control.Lens.Getter (view)

data AppConfig = AppConfig
    { databaseFilepath :: Maybe String
    }

appconfig :: Parser AppConfig
appconfig =
    AppConfig <$>
    (optional $
     Builder.option
         Builder.str
         (long "database" <> metavar "DATABASE" <>
          help "Filepath to notmuch database"))

purebred :: UserConfiguration -> IO ()
purebred config = do
    appconf <- execParser opts
    let
      setDB = (maybe id (const . pure) (databaseFilepath appconf))
      cfg' = over (confNotmuch . nmDatabase) setDB config
    buildLaunch `catch`
        \e -> hPrint stderr (e :: IOException) >> hFlush stderr
    launch cfg'
  where
    opts =
        info
            (appconfig <**> helper)
            (fullDesc <> progDesc "purebred" <>
             header "a search based, terminal mail user agent")

-- | Try to compile the config if it has changed and execute it
-- Note: This code is mostly borrowed from XMonad.Main.hs with the exception
-- that we're not handling any signals, but leave that up to System.Process for
-- good or worse.
buildLaunch :: IO ()
buildLaunch = do
    void $ recompile False
    configDir <- getPurebredConfigDir
    whoami <- getProgName
    args <- getArgs
    let bin = purebredCompiledName
    unless (whoami == bin) $
        createProcess (proc (configDir </> bin) args) >>=
        \(_,_,_,ph) -> waitForProcess ph >>=
        exitWith

launch :: UserConfiguration -> IO ()
launch cfg = do
    cfg' <- processConfig cfg
    s <- initialState cfg'
    void $ defaultMain (theApp s) s

processConfig :: UserConfiguration -> IO InternalConfiguration
processConfig cfg = do
    fp <- view (confNotmuch . nmDatabase) cfg
    ed <- view confEditor cfg
    pure $ cfg
      & set (confNotmuch . nmDatabase) fp
      & set confEditor ed

-- | Recompile the config file if it has changed based on the modification timestamp
-- Node: Mostly a XMonad.Main.hs rip-off.
recompile :: Bool -> IO Bool
recompile force = do
    configDir <- getPurebredConfigDir
    currDir <- getCurrentDirectory
    let binName = purebredCompiledName
        bin = configDir </> binName
        configSrc = configDir </> "config.hs"

    srcT <- getModTime configSrc
    binT <- getModTime bin

    if force || any (binT <) [srcT]
        then do
            status <- waitForProcess =<< compileGHC bin currDir configSrc
            pure (status == ExitSuccess)
        else pure True
  where
    getModTime f = catch (Just <$> getModificationTime f) (\(SomeException _) -> pure Nothing)

-- | Runs GHC to compile the given source file.
-- Note: This is also borrowed from XMonad.Main.hs, with the exception that I've
-- added the possibility to invoke stacks' GHC in case the user is in a stack
-- project. Copying configuration files around for development and (local)
-- testing could otherwise become a nuisance.
compileGHC :: String -> FilePath -> FilePath -> IO ProcessHandle
compileGHC bin cfgdir sourcePath = do
    withStack <- lookupEnv "WITHSTACK"
    if isJust withStack
        then runProcess
                 "stack"
                 [ "ghc"
                 , "--"
                 , "-threaded"
                 , "--make"
                 , sourcePath
                 , "-i"
                 , "-ilib"
                 , "-fforce-recomp"
                 , "-main-is"
                 , "main"
                 , "-v0"
                 , "-o"
                 , bin]
                 (Just cfgdir)
                 Nothing
                 Nothing
                 Nothing
                 Nothing
        else runProcess
                 "ghc"
                 [ "-threaded"
                 , "--make"
                 , sourcePath
                 , "-i"
                 , "-ilib"
                 , "-fforce-recomp"
                 , "-main-is"
                 , "main"
                 , "-v0"
                 , "-o"
                 , bin]
                 (Just cfgdir)
                 Nothing
                 Nothing
                 Nothing
                 Nothing

getPurebredConfigDir :: IO FilePath
getPurebredConfigDir = do
  cfgdir <- lookupEnv "PUREBRED_CONFIG_DIR"
  defaultcfgdir <- getUserConfigDir "purebred"
  pure $ fromMaybe defaultcfgdir cfgdir

purebredCompiledName :: String
purebredCompiledName = "purebred-" ++ arch ++ "-" ++ os
