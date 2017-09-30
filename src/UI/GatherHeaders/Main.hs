{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.GatherHeaders.Main where

import           Brick.Types                  (Padding (..), Widget)
import           Brick.Widgets.Core           (padRight, txt, vBox, vLimit,
                                               (<+>))
import qualified Brick.Widgets.Edit           as E
import           Control.Lens.Getter          (view)
import qualified Data.Text                    as T
import           UI.Draw.Main                 (editorDrawContent)
import           UI.Index.Main                (renderMailList)
import           UI.Status.Main               (statusbar)
import Types

drawInteractiveHeaders :: AppState -> [Widget Name]
drawInteractiveHeaders s = [ui]
  where
    inputBox = E.renderEditor editorDrawContent True (focusedEditor s)
    inputPrefix = padRight (Pad 1) $ getLabelForComposeState (view (asCompose . cFocus) s)
    ui = vBox [renderMailList s, statusbar s, inputPrefix <+> vLimit 1 inputBox]

focusedEditor :: AppState -> E.Editor T.Text Name
focusedEditor s =
    case view (asCompose . cFocus) s of
        AskFrom -> view (asCompose . cFrom) s
        AskTo -> view (asCompose . cTo) s
        AskSubject -> view (asCompose . cSubject) s

getLabelForComposeState :: ComposeState -> Widget Name
getLabelForComposeState AskFrom = txt "From:"
getLabelForComposeState AskTo = txt "To:"
getLabelForComposeState AskSubject = txt "Subject:"

