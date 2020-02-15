-- This file is part of purebred
-- Copyright (C) 2020 Fraser Tweedale and Róman Joost
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
module Purebred.System.Logging
  ( setupLogsink
  ) where

import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Monad (forever)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM (atomically, newTQueueIO, readTQueue, writeTQueue)
import System.IO (BufferMode(LineBuffering), IOMode(AppendMode), hSetBuffering, openFile)
import GHC.IO.Handle (Handle)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

-- | Creates the default log sink. The log function if we have a
-- filepath will log the given text with a timestamp prepended.
--
setupLogsink :: Maybe FilePath -> IO (T.Text -> IO ())
setupLogsink debugFile =
  case debugFile of
    Nothing -> pure $ \_ -> pure ()
    Just fp -> do
      h <- openFile fp AppendMode
      hSetBuffering h LineBuffering
      q <- newTQueueIO
      _ <- forkIO $ forever $ atomically (readTQueue q) >>= log_ h
      pure $ atomically . writeTQueue q

log_ :: Handle -> T.Text -> IO ()
log_ h logtext = do
  dateTimeNow <- getZonedTime
  let timestamp = T.pack $ formatTime defaultTimeLocale "%c" dateTimeNow
  T.hPutStrLn h $ timestamp <> " " <> logtext
