-- This file is part of purebred
-- Copyright (C) 2017-2019 Róman Joost and Fraser Tweedale
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

{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}

module Purebred.UI.Help.Main
  ( renderHelp
  , createKeybindingIndex
  , HelpIndex
  , KeybindingHelp(..)
  ) where

import Data.Function (on)
import Data.List (nubBy)
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Widgets.Core
       (Padding(..), viewport, hLimit, padLeft, padBottom, padRight, txt, (<=>),
        (<+>), vBox, withAttr, emptyWidget)
import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))
import Control.Lens (view, views, ifoldr)
import qualified Data.Map.Strict as M
import Data.Text (Text, singleton, intercalate, pack)
import Purebred.Config (helpTitleAttr, helpKeybindingAttr)
import Purebred.Types
import Purebred.UI.Utils (titleize, Titleize)

data KeybindingHelp =
  KeybindingHelp
    Text -- ^ keys
    Text -- ^ description
    String  -- ^ raw key event

type HelpIndex = M.Map Name [KeybindingHelp]

-- | Build a map between widget and it's configured key bindings
--
createKeybindingIndex :: Configuration -> HelpIndex
createKeybindingIndex cfg = M.fromList
  [ (ListOfThreads, views (confIndexView . ivBrowseThreadsKeybindings) kbGroup cfg)
  , (ComposeSubject, views (confComposeView . cvSubjectKeybindings) kbGroup cfg)
  , (ComposeBcc, views (confComposeView . cvBccKeybindings) kbGroup cfg)
  , (ComposeCc, views (confComposeView . cvCcKeybindings) kbGroup cfg)
  , (ComposeFrom, views (confComposeView . cvFromKeybindings) kbGroup cfg)
  , (ComposeTo, views (confComposeView . cvToKeybindings) kbGroup cfg)
  , (ConfirmDialog, views (confComposeView . cvConfirmKeybindings) kbGroup cfg)
  , (SaveToDiskPathEditor, views (confMailView . mvSaveToDiskKeybindings) kbGroup cfg)
  , (ScrollingMailViewFindWordEditor, views (confMailView . mvFindWordEditorKeybindings) kbGroup cfg)
  , (MailAttachmentPipeToEditor, views (confMailView . mvPipeToKeybindings) kbGroup cfg)
  , (MailAttachmentOpenWithEditor, views (confMailView . mvOpenWithKeybindings) kbGroup cfg)
  , (MailListOfAttachments, views (confMailView . mvMailListOfAttachmentsKeybindings) kbGroup cfg)
  , (ManageMailTagsEditor, views (confMailView . mvManageMailTagsKeybindings) kbGroup cfg)
  , (SearchThreadsEditor, views (confIndexView . ivSearchThreadsKeybindings) kbGroup cfg)
  , (ManageThreadTagsEditor, views (confIndexView . ivManageThreadTagsKeybindings) kbGroup cfg)
  , (ScrollingMailView, views (confMailView . mvKeybindings) kbGroup cfg)
  , (ScrollingHelpView, views (confHelpView . hvKeybindings) kbGroup cfg)
  , (ComposeListOfAttachments, views (confComposeView . cvListOfAttachmentsKeybindings) kbGroup cfg)
  , (ListOfFiles, views (confFileBrowserView . fbKeybindings) kbGroup cfg)
  , (ManageFileBrowserSearchPath, views (confFileBrowserView . fbSearchPathKeybindings) kbGroup cfg)
  ]

kbGroup :: [Keybinding v ctx] -> [KeybindingHelp]
kbGroup kbs = fmap createKeybindingHelp uniqKBs
  where
    uniqKBs = nubBy ((==) `on` view kbEvent) kbs

createKeybindingHelp :: Keybinding v ctx -> KeybindingHelp
createKeybindingHelp kb =
  let keys = view kbEvent kb
      actions = view (kbAction . aDescription) kb
   in KeybindingHelp (ppKbEvent keys) (intercalate " > " actions) (show keys)

renderHelp :: AppState -> Widget Name
renderHelp s =
  let index = createKeybindingIndex (view asConfig s)
   in viewport ScrollingHelpView T.Vertical $
      ifoldr renderKbGroup emptyWidget index

renderKbGroup ::
     Titleize a => a -> [KeybindingHelp] -> Widget Name -> Widget Name
renderKbGroup name kbs sibling =
  sibling
  <=> withAttr helpTitleAttr (padBottom (Pad 1) $ txt (titleize name))
  <=> padBottom (Pad 1) (vBox (renderKeybinding <$> kbs))

renderKeybinding :: KeybindingHelp -> Widget Name
renderKeybinding (KeybindingHelp keys actions _) =
  withAttr helpKeybindingAttr (hLimit 30 (padRight Max $ txt keys)) <+>
  padLeft (Pad 3) (txt actions)

ppKbEvent :: Event -> Text
ppKbEvent (EvKey k modifiers) = intercalate " + " $ (ppMod <$> modifiers) <> [ppKey k]
ppKbEvent _ = "<???>"

ppKey :: Key -> Text
ppKey KBS = "<BS>"
ppKey KBackTab = "<Shift>-<Tab>"
ppKey KEsc= "<Escape>"
ppKey KDel = "<Del>"
ppKey KEnd = "<End>"
ppKey KHome = "<Home>"
ppKey KRight = "<Right>"
ppKey KLeft = "<Left>"
ppKey KUp = "<Up>"
ppKey KDown = "<Down>"
ppKey KEnter = "<Enter>"
ppKey KPageUp = "<Page up>"
ppKey KPageDown = "<Page down>"
ppKey (KChar c) = ppChar c
ppKey (KFun n) = "<F" <> pack (show n) <> ">"
ppKey _ = "<???>"

ppChar :: Char -> Text
ppChar '\t' = "<Tab>"
ppChar ' ' = "Space"
ppChar c = singleton c

ppMod :: Modifier -> Text
ppMod MMeta = "<Meta>"
ppMod MAlt = "<Alt>"
ppMod MShift = "<Shift>"
ppMod MCtrl = "<Ctrl>"
