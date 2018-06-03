{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.GatherHeaders.Main (drawInteractiveHeaders) where

import Control.Lens (view)
import qualified Data.Text as T

import Brick.Types (Widget)
import qualified Brick.Widgets.Edit as E

import Types
import UI.Draw.Main (renderEditorWithLabel)
import UI.Utils (focusedViewWidget, titleize)

drawInteractiveHeaders :: AppState -> Widget Name
drawInteractiveHeaders s = renderEditorWithLabel (titleize $ focusedViewWidget s ComposeFrom) True (focusedEditor s)

focusedEditor :: AppState -> E.Editor T.Text Name
focusedEditor s =
    let focused = focusedViewWidget s ComposeFrom
    in case focused of
           ComposeFrom -> view (asCompose . cFrom) s
           ComposeTo -> view (asCompose . cTo) s
           _ -> view (asCompose . cSubject) s
