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

module Purebred.System
  ( tryIO
  , exceptionToError
  ) where

import Control.Exception (IOException, try)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)

import Error

-- | "Try" a computation but return a Purebred error in the exception case
tryIO :: (MonadError Error m, MonadIO m) => IO a -> m a
tryIO m = liftIO (try m) >>= either (throwError . exceptionToError) pure

exceptionToError :: IOException -> Error
exceptionToError = ProcessError . show
