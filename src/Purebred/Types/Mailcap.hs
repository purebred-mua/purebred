-- This file is part of purebred
-- Copyright (C) 2017-2021 Róman Joost and Fraser Tweedale
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
  ) where

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
import GHC.Generics

import Control.DeepSeq (NFData)
import Control.Lens (Lens', Traversal', filtered, lens, traversed, view)
import Control.Monad.Except (MonadError, MonadIO)
import qualified Data.ByteString as B
import System.Process.Typed (ProcessConfig, proc, shell)

import Data.MIME (ContentType)

import Purebred.System.Process
  ( EntityCommand(..), TempfileOnExit
  , handleExitCodeThrow, tmpfileResource, tryReadProcessStdout
  )
import Purebred.Types.Error (Error)

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
  -> EntityCommand m FilePath
mailcapHandlerToEntityCommand mh =
  EntityCommand
    handleExitCodeThrow
    (tmpfileResource (view mhKeepTemp mh))
    (\_ fp -> toProcessConfigWithTempfile (view mhMakeProcess mh) fp)
    tryReadProcessStdout

toProcessConfigWithTempfile :: MakeProcess -> FilePath -> ProcessConfig () () ()
toProcessConfigWithTempfile (Shell cmd) fp = shell (toList cmd <> " " <> fp)
toProcessConfigWithTempfile (Process cmd args) fp = proc (toList cmd) (args <> [fp])
