-- This file is part of purebred
-- Copyright (C) 2019 Róman Joost
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
{-# LANGUAGE RankNTypes #-}

{- | Module providing asynchronous validation for input from widgets -} 
module UI.Validation
  ( dispatchValidation
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Lens (Lens', set, view)
import Brick.BChan (writeBChan)

import Types


-- | Schedules validation by sending a PurebredEvent.
--
-- We fork a thread to send an event to the application which carries
-- the lens to set the error as well as the error itself. To avoid
-- setting an error on every key stroke, the thread is killed if we
-- find a thread id already set and a new thread is scheduled.
--
dispatchValidation ::
     (a -> Maybe UserMessage)  -- ^ validation function
  -> Lens' AppState (Maybe UserMessage)
  -> a
  -> AppState
  -> IO AppState
dispatchValidation fx l a s =
  let go = maybe schedule (\t -> killThread t *> schedule) . view (asAsync . aValidation)
      chan = view (asConfig . confBChan) s
      schedule =
        forkIO (sleepMs 500 >> writeBChan chan (InputValidated l (fx a)))
   in do tid <- go s
         pure $ set (asAsync . aValidation) (Just tid) s

sleepMs :: Int -> IO ()
sleepMs n = threadDelay (n * 1000)
