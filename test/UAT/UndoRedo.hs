-- This file is part of purebred
-- Copyright (C) 2026 Róman Joost
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

module UAT.UndoRedo
  ( testUndoRedo
  ) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import Test.Tasty.Tmux
import UAT.Common

testUndoRedo :: PurebredTestCase
testUndoRedo = purebredTmuxSession "undo and redo a tag operation" $
  \step -> do
    startApplication

    step "nothing to undo or redo on a fresh session"
    snapshot
    assertRegexS (undoRedoIndicator 0 0)

    -- use the configured 'a' (archive) short cut from the config to
    -- tag the thread with +archive
    step "archive the selected thread"
    sendKeys "a" (Substring "archive") >>= put
    assertRegexS (undoRedoIndicator 1 0)

    step "undo reverts the change and moves it onto the redo stack"
    sendKeys "u" (Not (Substring "archive")) >>= put
    assertRegexS (undoRedoIndicator 0 1)

    step "redo re-applies the change"
    sendKeys "C-r" (Substring "archive") >>= put
    assertRegexS (undoRedoIndicator 1 0)

undoRedoIndicator :: Int -> Int -> B.ByteString
undoRedoIndicator nundo nredo = T.encodeUtf8 . T.pack $
  "\x21b6 " <> show nundo <> " " <> show nredo <> " \x21b7"
