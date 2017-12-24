{-# LANGUAGE OverloadedStrings #-}
-- | module for drawing main window widgets
module UI.Draw.Main where

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core (fill, txt, vLimit, emptyWidget, padRight, (<+>))
import qualified Brick.Widgets.Edit as E
import Control.Lens (view)
import qualified Data.Text as T
import Types

fillLine :: Widget Name
fillLine = vLimit 1 (fill ' ')

editorDrawContent :: [T.Text] -> Widget Name
editorDrawContent st = txt $ T.unlines st

getModeTitle :: Mode -> Widget Name
getModeTitle GatherHeadersFrom = txt "From:"
getModeTitle GatherHeadersTo = txt "To:"
getModeTitle GatherHeadersSubject = txt "Subject:"
getModeTitle ManageThreadTags = txt "+Add -Remove Labels:"
getModeTitle ManageMailTags = txt "+Add -Remove Labels:"
getModeTitle SearchThreads = txt "Query:"
getModeTitle _ = emptyWidget

-- | Renders editor with a label on the left restricted to one line
renderEditorWithLabel :: AppState
                      -> Bool  -- ^ editor focus
                      -> E.Editor T.Text Name  -- ^ editor widget
                      -> Widget Name
renderEditorWithLabel s hasFocus e =
  let inputW = E.renderEditor editorDrawContent hasFocus e
      labelW = padRight (Pad 1) (getModeTitle (view asAppMode s))
  in labelW <+> vLimit 1 inputW
