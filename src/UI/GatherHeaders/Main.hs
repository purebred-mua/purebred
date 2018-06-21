{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.GatherHeaders.Main (drawFrom, drawTo, drawSubject) where

import Control.Lens (view)

import Brick.Types (Widget)

import Types
import UI.Draw.Main (renderEditorWithLabel)
import UI.Utils (focusedViewWidget)

drawFrom :: AppState -> Widget Name
drawFrom s = renderEditorWithLabel "From:" (ComposeFrom == focusedViewWidget s ComposeFrom) (view (asCompose . cFrom) s)

drawTo :: AppState -> Widget Name
drawTo s = renderEditorWithLabel "To:" (ComposeTo == focusedViewWidget s ComposeFrom) (view (asCompose . cTo) s)

drawSubject :: AppState -> Widget Name
drawSubject s = renderEditorWithLabel "Subject:" (ComposeSubject == focusedViewWidget s ComposeFrom) (view (asCompose . cSubject) s)
