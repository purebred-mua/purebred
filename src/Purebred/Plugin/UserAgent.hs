-- This file is part of purebred
-- Copyright (C) 2021 Fraser Tweedale
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
{-# LANGUAGE OverloadedStrings #-}

{-| Add a User-Agent header to outgoing messages. -}
module Purebred.Plugin.UserAgent (plugin) where

import Control.Lens (set, view)

import Data.MIME (headerText)

import Purebred.Plugin
import Purebred.Version (version, userAgent)
import Types

plugin :: Plugin (PreSendHook CanReadConfig)
plugin = Plugin "UserAgent" version (PreSendHook hook)
  where
  hook msg = do
    charsets <- view confCharsets
    pure $ set (headerText charsets "User-Agent") (Just userAgent) msg
