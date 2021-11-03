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

{- |

Event types and optics for Purebred.

-}
module Purebred.Types.Event
  (
    PurebredEvent(..)

  , Generation
  , firstGeneration
  , nextGeneration

  , UserMessage(..)
  , umContext
  , umSeverity
  , MessageSeverity(..)
  ) where

import Control.Lens (Lens', lens)
import qualified Data.Text as T

import Purebred.Types.Error (Error)
import Purebred.Types.UI (Name)

-- | A serial number that can be used to match (or ignore as
-- irrelevant) asynchronous events to current application state.
--
-- Use the 'Eq' and 'Ord' instances to compare generations.  The
-- constructor is hidden; use 'firstGeneration' as the first
-- generation, and use 'nextGeneration' to monotonically increment
-- it.
--
newtype Generation = Generation Integer
  deriving (Eq, Ord)

firstGeneration :: Generation
firstGeneration = Generation 0

nextGeneration :: Generation -> Generation
nextGeneration (Generation n) = Generation (succ n)

-- | Purebred event type.  In the future we can abstract this over
-- a custom event type to allow plugins to define their own events.
-- But I've YAGNI'd it for now because it will require an event
-- type parameter on 'AppState', which will be a noisy change.
--
data PurebredEvent
  = NotifyNumThreads Int Generation
  | NotifyNewMailArrived Int
  | InputValidated (Maybe UserMessage) -- ^ Event used for real time validation

data MessageSeverity
  = Error Error
  | Warning T.Text
  | Info T.Text
  deriving (Eq, Show)

-- | UI feedback shown to the user.
-- Uses context and severity to control visibility and importance.
data UserMessage = UserMessage
  { _umContext:: Name
  , _umSeverity :: MessageSeverity
  }
  deriving (Eq, Show)

umContext :: Lens' UserMessage Name
umContext = lens _umContext (\m x -> m { _umContext = x })

umSeverity :: Lens' UserMessage MessageSeverity
umSeverity = lens _umSeverity (\m x -> m { _umSeverity = x })
