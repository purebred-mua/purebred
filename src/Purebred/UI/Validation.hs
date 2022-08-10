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

{-# LANGUAGE FlexibleContexts #-}

{- | Asynchronous validation for input from widgets -}
module Purebred.UI.Validation
  ( dispatchValidation
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (MonadState)
import Control.Lens (assign, use)
import Brick.BChan (writeBChan)

import Purebred.Types


-- | Schedules validation by sending a PurebredEvent.
--
-- We fork a thread to send an event to the application which carries
-- the lens to set the error as well as the error itself. To avoid
-- setting an error on every key stroke, the thread is killed if we
-- find a thread id already set and a new thread is scheduled.
--
dispatchValidation
  :: (MonadIO m, MonadState AppState m)
  => (a -> Maybe UserMessage)  -- ^ validation function
  -> a
  -> m ()
dispatchValidation fx a = do
  chan <- use bChan
  mAsync <- use (asAsync . aValidation)
  let
    go = maybe schedule (\t -> killThread t *> schedule) mAsync
    schedule = forkIO $ sleepMs 500 *> writeBChan chan (InputValidated (fx a))
  tid <- liftIO go
  assign (asAsync . aValidation) (Just tid)

sleepMs :: Int -> IO ()
sleepMs n = threadDelay (n * 1000)
