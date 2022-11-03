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

Brick attribute names used in the Purebred UI.

-}
module Purebred.UI.Attr where

import qualified Brick.AttrMap as A
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E

-- ** State Attributes used to indicate list item state
-- List attributes are generated based on three possible states:
--
--   * selected (first)
--   * toggled
--   * new (last)
--
-- That means that for example, any new list items will inherit colour
-- definitions from selected and toggled attributes.
listStateSelectedAttr :: A.AttrName
listStateSelectedAttr = A.attrName "selected"

listStateNewmailAttr :: A.AttrName
listStateNewmailAttr = A.attrName "newmail"

listStateToggledAttr :: A.AttrName
listStateToggledAttr = A.attrName "toggled"

-- ** Widget Attributes
--
defaultAttr :: A.AttrName
defaultAttr = A.attrName "default"

mailViewAttr :: A.AttrName
mailViewAttr = A.attrName "mailview"

statusbarAttr :: A.AttrName
statusbarAttr = A.attrName "statusbar"

statusbarErrorAttr :: A.AttrName
statusbarErrorAttr = statusbarAttr <> A.attrName "error"

statusbarInfoAttr :: A.AttrName
statusbarInfoAttr = statusbarAttr <> A.attrName "info"

statusbarWarningAttr :: A.AttrName
statusbarWarningAttr = statusbarAttr <> A.attrName "warning"

editorAttr :: A.AttrName
editorAttr = E.editAttr

editorFocusedAttr :: A.AttrName
editorFocusedAttr = E.editFocusedAttr

editorErrorAttr :: A.AttrName
editorErrorAttr = editorAttr <> A.attrName "error"

editorLabelAttr :: A.AttrName
editorLabelAttr = editorAttr <> A.attrName "label"

listAttr :: A.AttrName
listAttr = L.listAttr

-- Note: Brick exports a L.listSelectedAttr, yet in order to make our
-- use of our listState attributes consistent across the application
-- we need to use our own listState attributes.
listSelectedAttr :: A.AttrName
listSelectedAttr = L.listAttr <> listStateSelectedAttr

listNewMailAttr :: A.AttrName
listNewMailAttr = L.listAttr <> listStateNewmailAttr

listSelectedNewmailAttr :: A.AttrName
listSelectedNewmailAttr = L.listSelectedAttr <> listStateNewmailAttr

listToggledAttr :: A.AttrName
listToggledAttr = L.listAttr <> listStateToggledAttr

listSelectedToggledAttr :: A.AttrName
listSelectedToggledAttr = listStateSelectedAttr <> listToggledAttr

mailAttr :: A.AttrName
mailAttr = A.attrName "mail"

mailTagAttr :: A.AttrName
mailTagAttr = mailAttr <> A.attrName "tag"

mailTagToggledAttr :: A.AttrName
mailTagToggledAttr = mailTagAttr <> listStateToggledAttr

mailAuthorsAttr :: A.AttrName
mailAuthorsAttr = mailAttr <> A.attrName "authors"

mailNewmailAuthorsAttr :: A.AttrName
mailNewmailAuthorsAttr = mailAuthorsAttr <> listStateNewmailAttr

mailToggledAuthorsAttr :: A.AttrName
mailToggledAuthorsAttr = mailAuthorsAttr <> listStateToggledAttr

mailSelectedAuthorsAttr :: A.AttrName
mailSelectedAuthorsAttr = mailAuthorsAttr <> listStateSelectedAttr

mailSelectedNewmailAuthorsAttr :: A.AttrName
mailSelectedNewmailAuthorsAttr = mailAuthorsAttr <> listStateSelectedAttr <> listStateNewmailAttr

mailSelectedToggledAuthorsAttr :: A.AttrName
mailSelectedToggledAuthorsAttr = mailSelectedAuthorsAttr <> listStateToggledAttr

headerAttr :: A.AttrName
headerAttr = A.attrName "header"

headerKeyAttr :: A.AttrName
headerKeyAttr = headerAttr <> A.attrName "key"

headerValueAttr :: A.AttrName
headerValueAttr = headerAttr <> A.attrName "value"

helpAttr :: A.AttrName
helpAttr = A.attrName "help"

helpTitleAttr :: A.AttrName
helpTitleAttr = helpAttr <> A.attrName "title"

helpKeybindingAttr :: A.AttrName
helpKeybindingAttr = helpAttr <> A.attrName "keybinding"


textMatchHighlightAttr :: A.AttrName
textMatchHighlightAttr = A.attrName "match"

currentTextMatchHighlightAttr :: A.AttrName
currentTextMatchHighlightAttr = textMatchHighlightAttr <> A.attrName "current"

mailbodyAttr :: A.AttrName
mailbodyAttr = A.attrName "mailbody"

mailbodySourceAttr :: A.AttrName
mailbodySourceAttr = mailbodyAttr <> A.attrName "source"
