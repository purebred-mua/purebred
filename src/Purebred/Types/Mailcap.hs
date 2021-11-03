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
  , TempfileOnExit(..)

  , EntityCommand(..)
  , ccResource
  , ccRunProcess
  , ccAfterExit
  , ccEntity
  , ccProcessConfig
  , ResourceSpec(..)
  , rsAcquire
  , rsUpdate
  , rsFree
  ) where

import Data.List.NonEmpty (NonEmpty)
import GHC.Generics
import System.Exit (ExitCode(..))

import Control.DeepSeq (NFData)
import Control.Lens (Lens', Traversal', filtered, lens, traversed, view)
import Control.Monad.Except (MonadError, MonadIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import System.Process.Typed (ProcessConfig)

import Data.MIME (ContentType)

import Purebred.Types.Error (Error)
import Purebred.Types.IFC (Tainted)

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

data TempfileOnExit
  = KeepTempfile
  | DiscardTempfile
  deriving (Generic, NFData)

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


-- | Command configuration which is bound to an acquired resource
-- (e.g. a tempfile) filtered through an external command. The
-- resource may or may not be cleaned up after the external command
-- exits.
data EntityCommand m a = EntityCommand
  { _ccAfterExit :: (ExitCode, Tainted L.ByteString) -> a -> m T.Text
  , _ccResource :: ResourceSpec m a
  , _ccProcessConfig :: B.ByteString -> a -> ProcessConfig () () ()
  , _ccRunProcess :: ProcessConfig () () () -> m ( ExitCode
                                                   , Tainted L.ByteString)
  , _ccEntity :: B.ByteString
  -- ^ The decoded Entity
  }

ccAfterExit ::
     (MonadIO m, MonadError Error m)
  => Lens' (EntityCommand m a) ((ExitCode, Tainted L.ByteString) -> a -> m T.Text)
ccAfterExit = lens _ccAfterExit (\cc x -> cc {_ccAfterExit = x})

ccEntity :: Lens' (EntityCommand m a) B.ByteString
ccEntity = lens _ccEntity (\cc x -> cc {_ccEntity = x})

ccProcessConfig ::
     Lens' (EntityCommand m a) (B.ByteString -> a -> ProcessConfig () () ())
ccProcessConfig = lens _ccProcessConfig (\cc x -> cc {_ccProcessConfig = x})

ccResource ::
     (MonadIO m, MonadError Error m)
  => Lens' (EntityCommand m a) (ResourceSpec m a)
ccResource = lens _ccResource (\cc x -> cc {_ccResource = x})

ccRunProcess ::
     (MonadError Error m, MonadIO m)
  => Lens' (EntityCommand m a) (ProcessConfig () () () -> m ( ExitCode
                                                            , Tainted L.ByteString))
ccRunProcess = lens _ccRunProcess (\cc x -> cc {_ccRunProcess = x})
