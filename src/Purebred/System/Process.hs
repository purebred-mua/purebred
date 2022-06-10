-- This file is part of purebred
-- Copyright (C) 2019-2021 Róman Joost
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

{-# LANGUAGE FlexibleContexts #-}

module Purebred.System.Process
  ( tryReadProcessStderr
  , tryReadProcessStdout
  , handleIOException
  , handleExitCodeThrow
  , handleExitCodeTempfileContents
  , outputToText
  , Purebred.System.Process.readProcess
  , tmpfileResource
  , draftFileResoure
  , emptyResource
  , toProcessConfigWithTempfile
  , runEntityCommand
  , createDraftFilePath
  , createSentFilePath
  -- * Re-exports from @System.Process.Typed@
  , ProcessConfig
  , proc
  , shell
  , setStdin
  , closed
  , byteStringInput
  ) where

import Data.Bifunctor (bimap)
import Data.Functor (($>))
import Control.Exception (IOException)
import Control.Monad.Catch (bracket, MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError)
import Control.Lens (_2, over, view)
import System.Process.Typed
import System.IO.Temp (emptyTempFile, emptySystemTempFile)
import System.FilePath ((</>))
import System.Directory (removeFile, createDirectoryIfMissing)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import Data.Char (isControl, isSpace)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Purebred.Types
import Purebred.System (tryIO, exceptionToError)
import Purebred.Types.Error
import Purebred.Types.IFC
import Purebred.UI.Notifications (setUserMessage, makeError)


-- | Handler to handle exit failures and possibly showing an error in the UI.
handleExitCodeThrow ::
     (MonadError Error m, MonadIO m)
  => (ExitCode, Tainted LB.ByteString)
  -> a
  -> m T.Text
handleExitCodeThrow (ExitFailure e, out) _ =
  throwError $ ProcessError (show e <> ": " <> T.unpack (outputToText out))
handleExitCodeThrow (ExitSuccess, out) _ = pure (outputToText out)

handleExitCodeTempfileContents ::
     (MonadError Error m, MonadIO m)
  => (ExitCode, Tainted LB.ByteString)
  -> FilePath
  -> m T.Text
handleExitCodeTempfileContents (ExitFailure e, out) _ =
  throwError $ ProcessError (show e <> ": " <> T.unpack (outputToText out))
handleExitCodeTempfileContents (ExitSuccess, _) tempfile = tryIO $ T.readFile tempfile

-- | Convert tained output from a 'readProcess' function to T.Text for
-- display
outputToText :: Tainted LB.ByteString -> T.Text
outputToText = untaint (sanitiseText . decodeLenient . LB.toStrict)

-- | Handle only IOExceptions, everything else is fair game.
handleIOException :: AppState -> IOException -> IO AppState
handleIOException s = pure . flip (setUserMessage . makeError StatusBar) s . exceptionToError

-- | Try running a process given by the `FilePath` and catch an IOExceptions.
-- This is to avoid a crashing process also take down the running Brick program.
--
-- Returns the exit code and the standard error output.
--
tryReadProcessStderr ::
     (MonadError Error m, MonadIO m)
  => ProcessConfig stdoutIgnored stderr stdin
  -> m (ExitCode, Tainted LB.ByteString)
tryReadProcessStderr pc = over _2 taint <$> tryIO (readProcessStderr pc)

tryReadProcessStdout ::
     (MonadError Error m, MonadIO m)
  => ProcessConfig stdout stderrIgnored stdin
  -> m (ExitCode, Tainted LB.ByteString)
tryReadProcessStdout pc = over _2 taint <$> tryIO (readProcessStdout pc)

-- | Run process, returning stdout and stderr as @ByteString@.
readProcess
  :: (MonadIO m)
  => ProcessConfig stdin stdoutIgnored stderrIgnored
  -> m (ExitCode, Tainted LB.ByteString, Tainted LB.ByteString)
readProcess = (fmap . fmap) (bimap taint taint) System.Process.Typed.readProcess

runEntityCommand ::
     (MonadMask m, MonadError Error m, MonadIO m)
  => EntityCommand m a
  -> m T.Text
runEntityCommand cmd =
  let acquire = view (ccResource . rsAcquire) cmd
      update = view (ccResource . rsUpdate) cmd
      free = view (ccResource . rsFree) cmd
      run = view ccRunProcess cmd
      afterExit = view ccAfterExit cmd
   in bracket
        (acquire >>= \tmpfile -> update tmpfile (view ccEntity cmd) $> tmpfile)
        free
        (\tmpfile ->
           run (view ccProcessConfig cmd (view ccEntity cmd) tmpfile) >>=
           flip afterExit tmpfile)

tmpfileResource ::
     (MonadIO m, MonadError Error m)
  => TempfileOnExit
  -> ResourceSpec m FilePath
tmpfileResource tmpOnExit =
  let cleanUp KeepTempfile = mempty
      cleanUp _ = removeFile
   in ResourceSpec
        (tryIO $ emptySystemTempFile "purebred")
        (tryIO . cleanUp tmpOnExit)
        (\fp -> tryIO . B.writeFile fp)

emptyResource :: (MonadIO m, MonadError Error m) => ResourceSpec m ()
emptyResource =
  ResourceSpec (pure mempty) (\_ -> pure mempty) (\_ _ -> pure mempty)

-- | Uses a maildir filename template and stores the temporary file in
-- the drafts folder.
draftFileResoure ::
     (MonadIO m, MonadError Error m)
  => FilePath -- ^ maildir path
  -> ResourceSpec m FilePath
draftFileResoure maildir =
  ResourceSpec
    (createDraftFilePath maildir)
    (tryIO . removeFile)
    (\fp -> tryIO . B.writeFile fp)

toProcessConfigWithTempfile :: MakeProcess -> FilePath -> ProcessConfig () () ()
toProcessConfigWithTempfile (Shell cmd) fp = shell (toList cmd <> " " <> fp)
toProcessConfigWithTempfile (Process cmd args) fp = proc (toList cmd) (args <> [fp])

-- | Generates a Maildir filename
-- see https://cr.yp.to/proto/maildir.html
maildirMessageFileTemplate :: MonadIO m => m FilePath
maildirMessageFileTemplate = do
  left <- liftIO $ formatTime defaultTimeLocale "%s" <$> getCurrentTime
  middle <- liftIO $ formatTime defaultTimeLocale "%p" <$> getCurrentTime
  right <- getHostname
  pure $ intercalate "." [left, middle, right]

getHostname :: (MonadIO m) => m String
getHostname = do
  (exitc, out, _) <- Purebred.System.Process.readProcess (proc "hostname" [])
  case exitc of
    ExitSuccess -> pure (decode out)
    ExitFailure _ -> pure "localhost"
  where
    decode =
      untaint
        (T.unpack .
         T.filter (\x -> not (isControl x || isSpace x)) .
         sanitiseText . decodeLenient . LB.toStrict)

-- | Create a temporary file in the drafts directory as a maildir
-- compliant filepath used for editing mail bodies.
-- Assumption: We don't cater for the case that the maildir does not exist!
-- The maildir is the notmuch database directory. If the maildir
-- wouldn't exist the entire application wouldn't run.
createDraftFilePath :: (MonadError Error m, MonadIO m) => FilePath -> m FilePath
createDraftFilePath maildir = touchMaildirFilePath (maildir </> "Drafts" </> "new")

createSentFilePath :: (MonadError Error m, MonadIO m) => FilePath -> m FilePath
createSentFilePath maildir = touchMaildirFilePath (maildir </> "Sent" </> "cur")

touchMaildirFilePath :: (MonadError Error m, MonadIO m) => FilePath -> m FilePath
touchMaildirFilePath maildir = tryIO $ do
    createDirectoryIfMissing True maildir
    maildirMessageFileTemplate >>= emptyTempFile maildir
