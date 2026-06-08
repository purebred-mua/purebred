-- This file is part of purebred
-- Copyright (C) 2026 Róman Joost
--
-- purebred is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE OverloadedStrings #-}

module UAT.Common
  ( PurebredTestCase
  , GlobalEnv(..)
  , purebredTmuxSession
  , envConfigDir
  , envMaildir
  , envNotmuchConfig
  , envSessionName
  , startApplication
  , mkTempDir
  , setUpPurebredConfig
  , precompileConfig
  , getSourceDirectory
  ) where

import Data.Functor (($>))
import System.IO.Temp
  ( createTempDirectory, getCanonicalTemporaryDirectory)
import Data.List (intercalate)
import System.FilePath.Posix
  ( (</>)
  , getSearchPath, isAbsolute, searchPathSeparator
  )
import System.Environment (lookupEnv, getEnvironment)
import System.Process.Typed
  (proc, readProcess_, runProcess_, setEnv)
import System.Directory
  ( copyFile, getCurrentDirectory, removeDirectoryRecursive
  )
import Control.Monad.IO.Class (liftIO)
import Control.Lens (Lens', lens)
import Control.Monad (void)
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadIO, MonadReader, runReaderT)

import Test.Tasty (TestName)
import Test.Tasty.Tmux

-- Global test environment (shared by all test cases)
newtype GlobalEnv = GlobalEnv FilePath

type PurebredTestCase = TestCase GlobalEnv

purebredTmuxSession ::
  TestName
  -> (forall m. (MonadReader Env m, MonadState Capture m, MonadIO m) => (String -> m()) -> m a)
  -> TestCase GlobalEnv
purebredTmuxSession = withTmuxSession setUp tearDown

-- Session test environment
data Env = Env
  { _envConfigDir :: FilePath
  , _envMaildir :: FilePath
  , _envNotmuchConfig :: FilePath
  , _envSessionName :: String
  }

instance HasTmuxSession Env where
  tmuxSession = envSessionName

-- | Session-specific config dir
envConfigDir :: Lens' Env FilePath
envConfigDir = lens _envConfigDir (\s b -> s { _envConfigDir = b })

envMaildir :: Lens' Env FilePath
envMaildir = lens _envMaildir (\s b -> s { _envMaildir = b })

envNotmuchConfig :: Lens' Env FilePath
envNotmuchConfig = lens _envNotmuchConfig (\s b -> s { _envNotmuchConfig = b })

envSessionName :: Lens' Env String
envSessionName = lens _envSessionName (\s b -> s { _envSessionName = b })

-- | Tear down a test session
tearDown :: Env -> IO ()
tearDown (Env confdir mdir _ _) = do
  removeDirectoryRecursive confdir
  removeDirectoryRecursive mdir

-- | Set up a test session.
setUp :: GlobalEnv -> TmuxSession -> IO Env
setUp (GlobalEnv globalConfigDir) sessionName = do
  maildir <- setUpTempMaildir
  nmCfg <- setUpNotmuchCfg maildir
  setUpNotmuch nmCfg

  confdir <- mkTempDir
  runProcess_ $ proc "sh" ["-c", "cp -a " <> globalConfigDir <> "/* " <> confdir]

  flip runReaderT sessionName $ do
    -- a) Make the regex less color code dependent by setting the TERM to 'screen'.
    -- This can happen if different environments support more than 16 colours (e.g.
    -- background values > 37), while our CI environment only supports 16 colours.
    --
    -- Previously we used value "ansi".  But we changed this because
    -- "ansi" can have different capabilities on different platforms,
    -- including missing ones.  On the other hand, "screen" triggers
    -- special handling within vty.
    setEnvVarInSession "TERM" "screen"

    -- set the config dir
    setEnvVarInSession "PUREBRED_CONFIG_DIR" confdir
    setEnvVarInSession "NOTMUCH_CONFIG" nmCfg

  pure $ Env confdir maildir nmCfg sessionName

precompileConfig :: FilePath -> IO ()
precompileConfig testdir = do
  env <- getEnvironment
  let systemEnv = ("PUREBRED_CONFIG_DIR", testdir) : env
      config = setEnv systemEnv $ proc "purebred" ["--version"]
  runProcess_ config

-- | Get the explicitly-specified source directory via SRCDIR
-- env var, or fall back to CWD.
getSourceDirectory :: IO FilePath
getSourceDirectory = lookupEnv "SRCDIR" >>= maybe getCurrentDirectory pure

setUpPurebredConfig :: FilePath -> IO ()
setUpPurebredConfig testdir = do
  c <- getSourceDirectory
  copyFile (c <> "/configs/purebred.hs") (testdir <> "/purebred.hs")
  copyFile (c <> "/configs/aliases") (testdir <> "/aliases")

mkTempDir :: IO FilePath
mkTempDir = getCanonicalTemporaryDirectory >>= flip createTempDirectory "purebredtest"

-- | Set up a temporary Maildir containing the test database
-- The returned directory contains the 'Maildir' subdirectory.
setUpTempMaildir :: IO FilePath
setUpTempMaildir = do
  basedir <- mkTempDir
  cwd <- getSourceDirectory
  runProcess_ $ proc "cp" ["-R", cwd <> "/test/data/Maildir", basedir]
  let mdir = basedir </> "Maildir"

  -- Rename files with maildir flags ; these had to be renamed (':' replaced
  -- with '_') to appease Hackage requirement that tarballs only contain
  -- filenames that are valid on both POSIX and Windows.  We have to fix the
  -- filenames here before using them.
  --
  -- In a Nix system the PATH environment may contain relative paths.
  -- For security reasons find(1) refuses to run when -execdir is given
  -- and PATH contains relative paths.  So we have to remove relative
  -- dirs from PATH.
  --
  path <- intercalate [searchPathSeparator]
          . filter isAbsolute
          <$> getSearchPath
  let
    f (k, _) | k == "PATH" = (k, path)
    f x = x
  env <- fmap f <$> getEnvironment
  runProcess_ $ setEnv env $ proc "find"
    [ mdir, "-name", "*_2,*"
    , "-execdir", "sh", "-c", "mv {} $(echo {} | sed s/_2,/:2,/)", ";"
    ]

  pure mdir

-- | run notmuch to create the notmuch database
-- Note: discard stdout which otherwise clobbers the test output
setUpNotmuch :: FilePath -> IO ()
setUpNotmuch notmuchcfg = void $ readProcess_ $ proc "notmuch" ["--config=" <> notmuchcfg, "new" ]

-- | Write a minimal notmuch config pointing to the given maildir.
-- Returns the path to the notmuch configuration file (which is
-- created under the given maildir directory).
--
setUpNotmuchCfg :: FilePath -> IO FilePath
setUpNotmuchCfg dir = do
  let cfgData = "[database]\npath=" <> dir <> "\n"
      cfgFile = dir <> "/notmuch-config"
  writeFile cfgFile cfgData $> cfgFile

-- | start the application
-- Note: this is currently defined as an additional test step for no good
-- reason.
startApplication :: (MonadReader Env m, MonadIO m) => m ()
startApplication = do
  srcdir <- liftIO getSourceDirectory
  tmuxSendKeys LiteralKeys ("cd " <> srcdir <> "\r")
  tmuxSendKeys InterpretKeys "purebred\r"
  void $ waitForCondition (Substring "Purebred: Item") defaultRetries defaultBackoff
