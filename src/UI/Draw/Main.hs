{-# LANGUAGE OverloadedStrings #-}
-- | module for drawing main window widgets
module UI.Draw.Main where

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core (fill, txt, vLimit, emptyWidget, padRight, (<+>))
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Types
import UI.Utils (focusedViewWidget)

fillLine :: Widget Name
fillLine = vLimit 1 (fill ' ')

editorDrawContent :: [T.Text] -> Widget Name
editorDrawContent st = txt $ T.unlines st

getTitle :: Name -> Widget Name
getTitle ComposeFrom = txt "From:"
getTitle ComposeTo = txt "To:"
getTitle ComposeSubject = txt "Subject:"
getTitle ManageThreadTagsEditor = txt "+Add -Remove Labels:"
getTitle ManageMailTagsEditor = txt "+Add -Remove Labels:"
getTitle SearchThreadsEditor = txt "Query:"
getTitle _ = emptyWidget

-- | Renders editor with a label on the left restricted to one line
renderEditorWithLabel :: AppState
                      -> Bool  -- ^ editor focus
                      -> E.Editor T.Text Name  -- ^ editor widget
                      -> Widget Name
renderEditorWithLabel s hasFocus e =
  let inputW = E.renderEditor editorDrawContent hasFocus e
      focused = focusedViewWidget s ListOfThreads
      labelW = padRight (Pad 1) (getTitle focused)
  in labelW <+> vLimit 1 inputW
