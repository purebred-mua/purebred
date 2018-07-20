{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | module for drawing main window widgets
module UI.Draw.Main where

import Brick.Types (Padding(..), Widget)
import Brick.AttrMap (AttrName)
import Brick.Widgets.Core (fill, txt, vLimit, padRight, (<+>), withAttr)
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Control.Lens (preview, view, to, _Just, has, _Left)
import Types
import UI.Utils (focusedViewWidget, getEditor)
import Config.Main (editorLabelAttr, editorAttr, editorFocusedAttr, editorErrorAttr)

fillLine :: Widget Name
fillLine = vLimit 1 (fill ' ')

editorDrawContent :: Bool -> [T.Text] -> Widget Name
editorDrawContent hasError st = let widget = txt $ T.unlines st
                                in if hasError then withAttr editorErrorAttr widget else widget

-- | Renders editor with a label on the left restricted to one line
renderEditorWithLabel :: T.Text
                      -> Name
                      -> AppState
                      -> Widget Name
renderEditorWithLabel label n s =
  let hasFocus = n == focusedViewWidget s ListOfThreads
      hasError = has (asError . _Just) s
      inputW = E.renderEditor (editorDrawContent hasError) hasFocus (getEditor n s)
      labelW = withAttr editorLabelAttr $ padRight (Pad 1) $ txt label
      eAttr = if hasFocus then editorFocusedAttr else editorAttr
  in labelW <+> withAttr eAttr (vLimit 1 inputW)
