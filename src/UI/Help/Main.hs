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
{-# LANGUAGE OverloadedStrings #-}
module UI.Help.Main (renderHelp) where

import Data.Function (on)
import Data.List (nubBy)
import Brick.Types (Padding(..), Widget)
import qualified Brick.Types as T
import Brick.Widgets.Core
       (viewport, hLimit, padLeft, padBottom, padRight, txt, (<=>),
        (<+>), vBox, withAttr)
import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))
import Control.Lens (view, views)
import Data.Text (Text, singleton, intercalate, pack)
import Config.Main (helpTitleAttr, helpKeybindingAttr)
import Types
import UI.Utils (titleize, Titleize)

renderHelp :: AppState -> Widget Name
renderHelp s = viewport ScrollingHelpView T.Vertical $ vBox
  [ views (asConfig . confIndexView . ivBrowseThreadsKeybindings) (renderKbGroup ListOfThreads) s
  , views (asConfig . confIndexView . ivSearchThreadsKeybindings) (renderKbGroup SearchThreadsEditor) s
  , views (asConfig . confIndexView . ivManageThreadTagsKeybindings) (renderKbGroup ManageThreadTagsEditor) s
  , views (asConfig . confMailView . mvKeybindings) (renderKbGroup ScrollingMailView) s
  , views (asConfig . confHelpView . hvKeybindings) (renderKbGroup ScrollingHelpView) s
  , views (asConfig . confComposeView . cvListOfAttachmentsKeybindings) (renderKbGroup ComposeListOfAttachments) s
  , views (asConfig . confFileBrowserView . fbKeybindings) (renderKbGroup ListOfFiles) s
  , views (asConfig . confFileBrowserView . fbSearchPathKeybindings) (renderKbGroup ManageFileBrowserSearchPath) s
  ]

renderKbGroup :: Titleize a => a -> [Keybinding v ctx] -> Widget Name
renderKbGroup name kbs =
  withAttr helpTitleAttr (padBottom (Pad 1) $ txt (titleize name))
  <=> padBottom (Pad 1) (vBox (renderKeybinding <$> uniqKBs))
  where
    uniqKBs = nubBy ((==) `on` view kbEvent) kbs

renderKeybinding :: Keybinding v ctx -> Widget Name
renderKeybinding kb = let keys = view kbEvent kb
                          actions = view (kbAction . aDescription) kb
                      in withAttr helpKeybindingAttr (hLimit 30 (padRight Max $ txt $ ppKbEvent keys))
                         <+> padLeft (Pad 3) (txt (intercalate " > " actions))

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
