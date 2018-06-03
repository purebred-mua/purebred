{-# LANGUAGE OverloadedStrings #-}
-- | module for drawing main window widgets
module UI.Draw.Main where

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core (fill, txt, vLimit, padRight, (<+>))
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Types

fillLine :: Widget Name
fillLine = vLimit 1 (fill ' ')

editorDrawContent :: [T.Text] -> Widget Name
editorDrawContent st = txt $ T.unlines st

-- | Renders editor with a label on the left restricted to one line
renderEditorWithLabel :: T.Text
                      -> Bool  -- ^ editor focus
                      -> E.Editor T.Text Name  -- ^ editor widget
                      -> Widget Name
renderEditorWithLabel label hasFocus e =
  let inputW = E.renderEditor editorDrawContent hasFocus e
      labelW = padRight (Pad 1) $ txt label
  in labelW <+> vLimit 1 inputW
