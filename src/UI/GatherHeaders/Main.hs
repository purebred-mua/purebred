{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.GatherHeaders.Main where

import           Brick.Types                  (Padding (..), Widget)
import           Brick.Widgets.Core           (padRight, vBox, vLimit, (<+>))

import qualified Brick.Widgets.Edit           as E
import           Control.Lens.Getter          (view)
import qualified Data.Text                    as T
import           UI.Draw.Main                 (editorDrawContent)
import           UI.Index.Main                (renderMailList)
import           UI.Status.Main               (statusbar)
import UI.ComposeEditor.Main (getLabelForComposeState)
import Types

drawInteractiveHeaders :: AppState -> [Widget Name]
drawInteractiveHeaders s = [ui]
  where
    inputBox = E.renderEditor editorDrawContent True (focusedEditor s)
    inputPrefix = padRight (Pad 1) $ getLabelForComposeState (view asAppMode s)
    ui = vBox [renderMailList s, statusbar s, inputPrefix <+> vLimit 1 inputBox]

focusedEditor :: AppState -> E.Editor T.Text Name
focusedEditor s =
    case view asAppMode s of
        GatherHeadersFrom -> view (asCompose . cFrom) s
        GatherHeadersTo -> view (asCompose . cTo) s
        _ -> view (asCompose . cSubject) s
