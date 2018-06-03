{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.GatherHeaders.Main (drawFrom, drawTo, drawSubject) where

import Control.Lens (view)

import Brick.Types (Widget)

import Types
import UI.Draw.Main (renderEditorWithLabel)

drawFrom :: AppState -> Widget Name
drawFrom s = renderEditorWithLabel "From:" True (view (asCompose . cFrom) s)

drawTo :: AppState -> Widget Name
drawTo s = renderEditorWithLabel "To:" True (view (asCompose . cTo) s)

drawSubject :: AppState -> Widget Name
drawSubject s = renderEditorWithLabel "Subject:" True (view (asCompose . cSubject) s)
