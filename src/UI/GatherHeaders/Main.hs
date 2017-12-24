{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.GatherHeaders.Main where

import Control.Lens (view)
import qualified Data.Text as T

import Brick.Types (Widget)
import Brick.Widgets.Core (vBox, txt)
import qualified Brick.Widgets.Edit as E

import Types
import UI.Draw.Main (renderEditorWithLabel)
import UI.Index.Main (renderMailList)
import UI.Status.Main (statusbar)

drawInteractiveHeaders :: AppState -> [Widget Name]
drawInteractiveHeaders s = [ui]
  where
    inputBox = renderEditorWithLabel s True (focusedEditor s)
    ui = vBox [renderMailList s, statusbar s, inputBox]

focusedEditor :: AppState -> E.Editor T.Text Name
focusedEditor s =
    case view asAppMode s of
        GatherHeadersFrom -> view (asCompose . cFrom) s
        GatherHeadersTo -> view (asCompose . cTo) s
        _ -> view (asCompose . cSubject) s

getLabelForComposeState :: Mode -> Widget Name
getLabelForComposeState GatherHeadersFrom = txt "From:"
getLabelForComposeState GatherHeadersTo = txt "To:"
getLabelForComposeState _ = txt "Subject:"
