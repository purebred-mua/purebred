-- This file is part of purebred
-- Copyright (C) 2017-2022 Róman Joost and Fraser Tweedale
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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

{- |

Types for handling message bodies with external programs.

-}
module Purebred.Types.Mailcap
  (
    MailcapHandler(..)
  , mhMakeProcess
  , mhCopiousoutput
  , mhKeepTemp
  , MakeProcess(..)
  , mpCommand
  , CopiousOutput(..)
  , isCopiousOutput
  , hasCopiousoutput

  , mailcapHandlerToEntityCommand

  -- * New API
  , PresentationContext(..)
  , Processor
  , ProcessingResult(..)
  , runProcessors

  -- ** Compatibility shims
  , mailcapHandlerProcessor
  ) where

import Data.Foldable (foldlM, toList)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromMaybe)
import Data.Monoid (First(..))
import GHC.Generics

import Control.DeepSeq (NFData)
import Control.Lens (Lens', Traversal', filtered, lens, traversed, view, views)
import Control.Monad.Catch (MonadMask)
import Control.Monad.Except (MonadError, MonadIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as Builder
import System.Process.Typed (ProcessConfig, proc, shell)

import Data.MIME
  ( MIMEMessage, MIME(Part), ContentType
  , body, buildBody, headers, contentType
  )

import Purebred.System.Process
  ( EntityCommand(..), TempfileOnExit
  , handleExitCodeThrow, tmpfileResource, tryReadProcessStdout
  , runEntityCommand
  )
import Purebred.Types.Error (Error)
import Purebred.Types.IFC (Tainted)

data MakeProcess
  = Shell (NonEmpty Char)
  | Process (NonEmpty Char)
            [String]
  deriving (Generic, NFData)

mpCommand :: Lens' MakeProcess (NonEmpty Char)
mpCommand f (Shell x) = fmap Shell (f x)
mpCommand f (Process x args) = fmap (\x' -> Process x' args) (f x)
{-# ANN mpCommand ("HLint: ignore Avoid lambda using `infix`" :: String) #-}

data CopiousOutput
  = CopiousOutput
  | IgnoreOutput
  deriving (Generic, NFData)

isCopiousOutput :: CopiousOutput -> Bool
isCopiousOutput CopiousOutput = True
isCopiousOutput _ = False

data MailcapHandler = MailcapHandler
  { _mhMakeProcess :: MakeProcess
  , _mhCopiousoutput :: CopiousOutput
  -- ^ output should be paged or made scrollable
  , _mhKeepTemp :: TempfileOnExit
  -- ^ Keep the temporary file if application spawns child and parent
  -- exits immediately (e.g. Firefox)
  } deriving (Generic, NFData)

mhMakeProcess :: Lens' MailcapHandler MakeProcess
mhMakeProcess = lens _mhMakeProcess (\h x -> h { _mhMakeProcess = x })

mhCopiousoutput :: Lens' MailcapHandler CopiousOutput
mhCopiousoutput = lens _mhCopiousoutput (\h x -> h { _mhCopiousoutput = x })

mhKeepTemp :: Lens' MailcapHandler TempfileOnExit
mhKeepTemp = lens _mhKeepTemp (\h x -> h { _mhKeepTemp = x })

hasCopiousoutput :: Traversal' [(ContentType -> Bool, MailcapHandler)] (ContentType -> Bool, MailcapHandler)
hasCopiousoutput = traversed . filtered (isCopiousOutput . view mhCopiousoutput . snd)

-- | Create an entity command which writes the entity to a tempfile,
-- runs the command given by the 'MailcapHandler' over it and grab the
-- stdout for later display.
--
mailcapHandlerToEntityCommand
  :: (MonadError Error m, MonadIO m)
  => MailcapHandler
  -> B.ByteString
  -> EntityCommand m FilePath (Tainted L.ByteString)
mailcapHandlerToEntityCommand mh =
  EntityCommand
    handleExitCodeThrow
    (tmpfileResource (view mhKeepTemp mh))
    (\_ fp -> toProcessConfigWithTempfile (view mhMakeProcess mh) fp)
    tryReadProcessStdout

toProcessConfigWithTempfile :: MakeProcess -> FilePath -> ProcessConfig () () ()
toProcessConfigWithTempfile (Shell cmd) fp = shell (toList cmd <> " " <> fp)
toProcessConfigWithTempfile (Process cmd args) fp = proc (toList cmd) (args <> [fp])


---
--- NEW MAILCAP IMPLEMENTATION
---

data PresentationContext
  = PresentationInternal
  -- ^ Display the entity in Purebred's paging viewer,
  -- using a 'BodyPresentation'.
  | PresentationExternal
  -- ^ Use an external program to present the entity.

-- | A presentation processor operates in two stages.
--
-- In the first stage, the processor determines if it can apply itself
-- to the given message or part, in the given 'PresentationContext'.
-- Checking the @Content-Type@ header is typical.  But it may do
-- artibrary, effectful processing, for example executing a mailcap
-- @test@ command.  The result is @Just action@, or @Nothing@ if the
-- processor cannot handle the given message or part.
--
-- The second stage @action@, if defined, processes the message and
-- returns a 'ProcessingResult'.  Depending on the result value,
-- processing may continue, or the value may be final.
--
-- The type uses separate type parameters for the effect context of each
-- of the stages.  This enables types to reflect that, e.g. the "test"
-- stage is pure whilst the "apply" stage is effectful.  Both @m@
-- and @m'@ must unify with Purebred's main monad transformer stack.
--
type Processor m m' =
  PresentationContext
  -> MIMEMessage
  -> m (Maybe (m' ProcessingResult))

data ProcessingResult
  = ContinueProcessing MIMEMessage
  -- ^ Return another 'MIMEMessage', which may be subject to further
  -- processing.  The message must be fundamentally transformed in
  -- some way, such that the same processor will not apply again.
  | DoneProcessing (Either MIMEMessage (Tainted L.ByteString))
  -- ^ Processing was completed, and yielded the given output.

-- | Given an ordered list of 'Processor', apply the first matching
-- processor.  If the result is 'ContinueProcessing', process the
-- resulting message (against the whole list of processors).
--
-- Processing stops upon a 'DoneProcessing' result, or when no
-- processor matches the message.
--
runProcessors
  :: (Monad m)
  => [Processor m m]
  -> PresentationContext
  -> MIMEMessage
  -> m (Either MIMEMessage (Tainted L.ByteString))
runProcessors processors ctx msg = do
  k <- getFirst <$> foldlM acc mempty processors
  case k of
    Nothing -> pure $ Left msg
    Just k' -> do
      r <- k'
      case r of
        ContinueProcessing msg' -> runProcessors processors ctx msg'
        DoneProcessing r'       -> pure r'
  where
    acc z check = (z <>) . First <$> check ctx msg

-- | Lift legacy 'MailcapHandler' into a 'Processor'
mailcapHandlerProcessor
  :: (MonadError Error m, MonadIO m, MonadMask m)
  => (ContentType -> Bool, MailcapHandler)
  -> Processor m m
mailcapHandlerProcessor (contentTypePredicate, handler) ctx msg
  | presCtxOk, views contentType contentTypePredicate msg
  = pure $ Just go
  | otherwise
  = pure Nothing
  where
    presCtxOk =
      case (ctx, view mhCopiousoutput handler) of
        (PresentationInternal, CopiousOutput) -> True
        (PresentationExternal, IgnoreOutput)  -> True
        _ -> False
    go =
      fmap (DoneProcessing . Right)
      . runEntityCommand
      $ mailcapHandlerToEntityCommand handler bodyBytes
    bodyBytes = case view body msg of
      Part partbody -> partbody  -- avoid detour through Builder
      _ ->
        fromMaybe mempty
        $ L.toStrict . Builder.toLazyByteString
          <$> buildBody (view headers msg) (view body msg)
