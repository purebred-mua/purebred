-- This file is part of purebred
-- Copyright (C) 2022 - Róman Joost
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
{-# LANGUAGE FlexibleContexts #-}

module Purebred.Storage.AddressBook
  ( -- * Synopsis
    -- $synopsis

    -- * API
    queryAddresses
  )
where

import qualified Data.Text as T

import Control.Lens
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)

import Data.IMF (Address)

import Purebred.Types.Error
import Purebred.Types.AddressBook
import Purebred.System (tryIO)

-- $synopsis
--
-- This module provides an API to retrieve Address information for Purebred

-- | query addresses matching given needle
--
queryAddresses :: (MonadError Error m, MonadIO m) => T.Text -> [AddressBook] -> m [Address]
queryAddresses needle = fmap concat . traverse (tryIO . flip (view addressBookSearch) needle)
