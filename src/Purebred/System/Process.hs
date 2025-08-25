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

{-# LANGUAGE DeriveAnyClass #-}

module Purebred.System.Process
  ( tryReadProcessStderr
  , tryReadProcessStdout
  , handleExitCodeThrow
  , handleExitCodeTempfileContents
  , outputToText
  , Purebred.System.Process.readProcess
  , tmpfileResource
  , TempfileOnExit(..)
  , draftFileResoure
  , emptyResource
  , runEntityCommand
  , createDraftFilePath
  , createSentFilePath
  -- * Resource setup and cleanup
  , ResourceSpec(..)
  , rsAcquire
  , rsUpdate
  , rsFree
  , EntityCommandAfterExit
  , EntityCommand(..)
  , ccResource
  , ccRunProcess
  , ccAfterExit
  , ccEntity
  , ccProcessConfig
  -- * Re-exports from @System.Process.Typed@
  , ProcessConfig
  , proc
  , shell
  , setStdin
  , closed
  , byteStringInput
  ) where

import GHC.Generics
import Control.DeepSeq (NFData)
import Data.Bifunctor (bimap)
import Data.Functor (($>))
import Control.Monad.Catch (bracket, MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError)
import Control.Lens (Lens, Lens', _2, lens, over, view)
import System.Process.Typed
import System.IO.Temp (emptyTempFile, emptySystemTempFile)
import System.FilePath ((</>))
import System.Directory (removeFile, createDirectoryIfMissing)
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import Data.Char (isControl, isSpace)
import Data.List (intercalate)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

import qualified Data.Text as T

import Purebred.System (tryIO)
import Purebred.Types.Error
import Purebred.Types.IFC
import Purebred.Types.String (decodeLenient)

-- | A bracket-style type for creating and releasing acquired resources (e.g.
-- temporary files). Note that extending this is perhaps not worth it and we
-- should perhaps look at ResourceT if necessary.
data ResourceSpec m a = ResourceSpec
  { _rsAcquire :: m a
 -- ^ acquire a resource (e.g. create temporary file)
  , _rsFree :: a -> m ()
 -- ^ release a resource (e.g. remove temporary file)
  , _rsUpdate :: a -> B.ByteString -> m ()
 -- ^ update the acquired resource with the ByteString obtained from serialising the WireEntity
  }

rsAcquire :: Lens' (ResourceSpec m a) (m a)
rsAcquire = lens _rsAcquire (\rs x -> rs {_rsAcquire = x})

rsFree :: Lens' (ResourceSpec m a) (a -> m ())
rsFree = lens _rsFree (\rs x -> rs {_rsFree = x})

rsUpdate :: Lens' (ResourceSpec m a) (a -> B.ByteString -> m ())
rsUpdate = lens _rsUpdate (\rs x -> rs {_rsUpdate = x})


-- | Action to run after the command terminates, but before resource
-- cleanup.  Inputs are:
--
-- * Exit status
-- * Process output.  Could be stdout or stderr depending on the
--   value of 'ccProcessConfig'
-- * @res@, the resource value
--
type EntityCommandAfterExit m res a =
  (ExitCode, Tainted LB.ByteString) -> res -> m a

-- | Command configuration which is bound to an acquired resource
-- @res@ (e.g. a tempfile) filtered through an external command.
--
-- Resource @res@ acquisition and cleanup is governed by the
-- 'ResourceSpec' (field 'ccResource').
--
-- The result value @a@ is produced by the 'EntityCommandAfterExit'
-- (field 'ccAfterExit').
--
data EntityCommand m res a = EntityCommand
  { _ccAfterExit :: EntityCommandAfterExit m res a
  , _ccResource :: ResourceSpec m res
  , _ccProcessConfig :: B.ByteString -> res -> ProcessConfig () () ()
  , _ccRunProcess :: ProcessConfig () () () -> m (ExitCode, Tainted LB.ByteString)
  , _ccEntity :: B.ByteString
  -- ^ The transfer-decoded Entity
  }

instance Functor m => Functor (EntityCommand m res) where
  fmap f = over ccAfterExit (fmap . fmap . fmap $ f)

ccAfterExit
  :: Lens
      (EntityCommand m res a) (EntityCommand m res b)
      (EntityCommandAfterExit m res a) (EntityCommandAfterExit m res b)
ccAfterExit = lens _ccAfterExit (\cc x -> cc {_ccAfterExit = x})

ccEntity :: Lens' (EntityCommand m res a) B.ByteString
ccEntity = lens _ccEntity (\cc x -> cc {_ccEntity = x})

ccProcessConfig
  :: Lens' (EntityCommand m res a) (B.ByteString -> res -> ProcessConfig () () ())
ccProcessConfig = lens _ccProcessConfig (\cc x -> cc {_ccProcessConfig = x})

ccResource :: Lens' (EntityCommand m res a) (ResourceSpec m res)
ccResource = lens _ccResource (\cc x -> cc {_ccResource = x})

ccRunProcess
  :: Lens'
      (EntityCommand m res a)
      (ProcessConfig () () () -> m (ExitCode , Tainted LB.ByteString))
ccRunProcess = lens _ccRunProcess (\cc x -> cc {_ccRunProcess = x})


data TempfileOnExit
  = KeepTempfile
  | DiscardTempfile
  deriving (Generic, NFData)


throwOnExitFailureOr
  :: (MonadError Error m)
  => (ExitCode, Tainted LB.ByteString)
  -> (Tainted LB.ByteString -> m b)
  -> m b
throwOnExitFailureOr (code, out) k = case code of
  ExitFailure e ->
    throwError $ ProcessError (show e <> ": " <> T.unpack (outputToText out))
  ExitSuccess  ->
    k out

-- | Handler to handle exit failures and possibly showing an error in the UI.
handleExitCodeThrow
  :: (MonadError Error m)
  => EntityCommandAfterExit m res (Tainted LB.ByteString)
handleExitCodeThrow r _ = throwOnExitFailureOr r pure

-- | Throw 'ProcessError' on abnormal exit, otherwise
-- return the contents at given file path.
handleExitCodeTempfileContents
  :: (MonadError Error m, MonadIO m)
  => EntityCommandAfterExit m FilePath (Tainted LB.ByteString)
handleExitCodeTempfileContents r path =
  throwOnExitFailureOr r (\_ -> tryIO $ taint <$> LB.readFile path)

-- | Convert tained output from a 'readProcess' function to T.Text for
-- display
outputToText :: Tainted LB.ByteString -> T.Text
outputToText = untaint (sanitiseText . decodeLenient . LB.toStrict)

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

runEntityCommand
  :: (MonadMask m, MonadError Error m)
  => EntityCommand m res a
  -> m a
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

emptyResource :: (MonadIO m) => ResourceSpec m ()
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
