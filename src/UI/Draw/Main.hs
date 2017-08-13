-- | module for drawing main window widgets
module UI.Draw.Main where

import           Brick.Types        (Widget)
import           Brick.Widgets.Core (fill, txt, vLimit)

import qualified Data.Text          as T
import Types (Name)

fillLine :: Widget Name
fillLine = vLimit 1 (fill ' ')

editorDrawContent :: [T.Text] -> Widget Name
editorDrawContent st = txt $ T.unlines st
