-- This file is part of purebred
-- Copyright (C) 2018 Róman Joost
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
{-# LANGUAGE TupleSections #-}
module Purebred.System.Directory where

import Control.Exception.Base (IOException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Except (MonadError, throwError)
import Control.Exception (try)
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import Data.List (sort)

import Error
import Types


listDirectory' :: (MonadError Error m, MonadIO m) => FilePath -> m [FileSystemEntry]
listDirectory' path = liftIO (try $ listDirectory path)
                      >>= either (throwError . convertError) (traverse (filePathToEntry path) >>= pure . fmap sort)
  where convertError :: IOException -> Error
        convertError = GenericError . show

filePathToEntry :: (MonadIO m) => FilePath -> FilePath -> m FileSystemEntry
filePathToEntry base filename = do
    exists <- liftIO $ doesDirectoryExist (base </> filename)
    pure $ if exists then Directory filename else File filename
