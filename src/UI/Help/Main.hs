{-# LANGUAGE OverloadedStrings #-}
module UI.Help.Main where

import Brick.Types (Padding(..), Widget)
import qualified Brick.Types as T
import Brick.Widgets.Core
       (viewport, hLimit, padLeft, padBottom, padRight, str, txt, (<=>),
        (<+>), emptyWidget, withAttr)
import Graphics.Vty.Input.Events (Event(..), Key(..), Modifier(..))
import Control.Lens (view, views)
import Data.Semigroup ((<>))
import Data.Text (Text, singleton, intercalate, pack)
import Config.Main (helpTitleAttr, helpKeybindingAttr)
import Types

drawHelp :: AppState -> [Widget Name]
drawHelp s = let tweak = views (asConfig . confIndexView . ivKeybindings) (renderKbGroup BrowseMail) s
                         <=> views (asConfig . confIndexView . ivSearchKeybindings) (renderKbGroup SearchMail) s
                         <=> views (asConfig . confMailView . mvIndexKeybindings) (renderKbGroup ViewMail) s
                         <=> views (asConfig . confMailView . mvKeybindings) (renderKbGroup ViewMail) s
                         <=> views (asConfig . confComposeView . cvKeybindings) (renderKbGroup ComposeEditor) s
                         <=> views (asConfig . confHelpView . hvKeybindings) (renderKbGroup Help) s
             in [viewport ScrollingHelpView T.Vertical tweak]

renderKbGroup :: Mode -> [Keybinding ctx a] -> Widget Name
renderKbGroup m kbs = emptyWidget
                      <=> withAttr helpTitleAttr (padBottom (Pad 1) $ txt (modeTitle m))
                      <=> padBottom (Pad 1) (foldl (\a x -> a <=> renderKeybinding x) emptyWidget kbs)

renderKeybinding :: Keybinding ctx a-> Widget Name
renderKeybinding kb = let keys = view kbEvent kb
                          actions = view (kbAction . aDescription) kb
                      in withAttr helpKeybindingAttr (hLimit 30 (padRight Max $ txt $ ppKbEvent keys))
                         <+> padLeft (Pad 3) (str actions)

modeTitle :: Mode -> Text
modeTitle BrowseMail = "Index of Mails"
modeTitle SearchMail = "Search Editor"
modeTitle ViewMail = "Mail Viewer"
modeTitle ComposeEditor = "Editor to Compose a new Mail"
modeTitle m = pack $ show m

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
