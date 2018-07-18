{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
-- | module for drawing main window widgets
module UI.Draw.Main where

import Brick.Types (Padding(..), Widget)
import Brick.Widgets.Core
       (fill, txt, vLimit, padRight, (<+>), withAttr, padLeft, hBox)
import qualified Brick.Widgets.Edit as E
import qualified Data.Text as T
import Data.Proxy
import Control.Lens (view)
import Types
import Config.Main (editorLabelAttr, editorAttr, editorFocusedAttr, statusbarAttr)
import UI.Views (focusedViewWidget)
import UI.Actions (HasName(..), HasEditor(..))

fillLine :: Widget Name
fillLine = vLimit 1 (fill ' ')

attachmentsHeader :: Widget Name
attachmentsHeader = withAttr statusbarAttr $ hBox [ padLeft (Pad 1) (txt "-- Attachments") , vLimit 1 (fill '-')]

editorDrawContent :: [T.Text] -> Widget Name
editorDrawContent st = txt $ T.unlines st

-- | Renders editor with a label on the left restricted to one line
renderEditorWithLabel ::
     (HasName n, HasEditor n) => Proxy n -> T.Text -> AppState -> Widget Name
renderEditorWithLabel p label s =
  let hasFocus = name p == focusedViewWidget s
      inputW = E.renderEditor editorDrawContent hasFocus (view (editorL p) s)
      labelW = withAttr editorLabelAttr $ padRight (Pad 1) $ txt label
      eAttr =
        if hasFocus
          then editorFocusedAttr
          else editorAttr
   in labelW <+> withAttr eAttr (vLimit 1 inputW)
