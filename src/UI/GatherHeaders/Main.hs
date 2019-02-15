{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.GatherHeaders.Main (drawFrom, drawTo, drawSubject) where

import Control.Lens (view)

import Brick.Types (Widget)

import Types
import UI.Draw.Main (renderEditorWithLabel)
import UI.Views (focusedViewWidget)

drawFrom :: AppState -> Widget Name
drawFrom s = renderEditorWithLabel "From:" (ComposeFrom == focusedViewWidget s) (view (asCompose . cFrom) s)

drawTo :: AppState -> Widget Name
drawTo s = renderEditorWithLabel "To:" (ComposeTo == focusedViewWidget s) (view (asCompose . cTo) s)

drawSubject :: AppState -> Widget Name
drawSubject s = renderEditorWithLabel "Subject:" (ComposeSubject == focusedViewWidget s) (view (asCompose . cSubject) s)
