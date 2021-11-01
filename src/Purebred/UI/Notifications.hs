-- This file is part of purebred
-- Copyright (C) 2021 Róman Joost
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

module Purebred.UI.Notifications (
  -- * Overview
  -- $overview

  -- * Setters on the AppState
  setUserMessage

  -- * Utilities
  , hasError

  -- * Factory functions
  , makeInfo
  , makeWarning
  , makeError

  -- * State Monad Setters
  , showError
  , showWarning
  , showInfo
  , showUserMessage
  ) where

import Control.Lens (view, set, assign)
import qualified Data.Text as T
import Control.Monad.State

import Purebred.Types
import Purebred.Types.Error

{- $overview

Notifications are one-line messages shown to the user. That can be
Errors, Warnings or simply acknowledgement of actions.

Note: If you want to make the 'UserMessage' visible between views in
the UI, use a context which is always displayed. At this point
'StatusBar' serves this purpose well.

-}

setUserMessage :: UserMessage -> AppState -> AppState
setUserMessage = set asUserMessage . Just

hasError :: AppState -> Bool
hasError = go . view asUserMessage
  where
    go (Just (UserMessage _ (Error _))) = True
    go _ = False

showError :: (MonadState AppState m) => Error -> m ()
showError = showUserMessage . makeError StatusBar

showWarning :: (MonadState AppState m) => T.Text -> m ()
showWarning = showUserMessage . makeWarning StatusBar

showInfo :: (MonadState AppState m) => T.Text -> m ()
showInfo = showUserMessage . makeInfo StatusBar

showUserMessage :: (MonadState AppState m) => UserMessage -> m ()
showUserMessage = assign asUserMessage . Just


makeInfo :: Name -> T.Text -> UserMessage
makeInfo n = UserMessage n . Info

makeWarning :: Name -> T.Text -> UserMessage
makeWarning n = UserMessage n . Warning

makeError :: Name -> Error -> UserMessage
makeError n = UserMessage n . Error
