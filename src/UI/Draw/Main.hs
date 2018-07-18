{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
-- | module for drawing main window widgets
module UI.Draw.Main where

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core (fill, txt, vLimit, padRight, (<+>), withAttr)
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Control.Lens (view)
import Types
import UI.Utils (focusedViewWidget)
import Config.Main (editorLabelAttr, editorAttr, editorFocusedAttr)

fillLine :: Widget Name
fillLine = vLimit 1 (fill ' ')

editorDrawContent :: [T.Text] -> Widget Name
editorDrawContent st = txt $ T.unlines st

-- | Renders editor with a label on the left restricted to one line
renderEditorWithLabel :: T.Text
                      -> Name
                      -> AppState
                      -> Widget Name
renderEditorWithLabel label n s =
  let hasFocus = n == focusedViewWidget s ListOfThreads
      inputW = E.renderEditor editorDrawContent hasFocus (getEditor n s)
      labelW = withAttr editorLabelAttr $ padRight (Pad 1) $ txt label
      eAttr = if hasFocus then editorFocusedAttr else editorAttr
  in labelW <+> withAttr eAttr (vLimit 1 inputW)

getEditor :: Name -> AppState -> E.Editor T.Text Name
getEditor ComposeFrom = view (asCompose . cFrom)
getEditor ComposeTo = view (asCompose . cTo)
getEditor ComposeSubject = view (asCompose . cSubject)
getEditor ManageMailTagsEditor = view (asMailIndex . miMailTagsEditor)
getEditor ManageThreadTagsEditor = view (asMailIndex . miThreadTagsEditor)
getEditor _ = view (asMailIndex . miSearchThreadsEditor)

