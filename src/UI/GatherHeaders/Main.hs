{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module UI.GatherHeaders.Main where

import qualified Brick.Main                   as M
import           Brick.Types                  (Padding (..), Widget)
import qualified Brick.Types                  as T
import           Brick.Widgets.Core           (padRight, txt, vBox, vLimit,
                                               (<+>))
import qualified Brick.Widgets.Edit           as E
import           Control.Lens.Getter          (view)
import           Control.Lens.Lens            (Lens')
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import Graphics.Vty (Event)
import           UI.Draw.Main                 (editorDrawContent)
import           UI.Index.Main                (renderMailList)
import           UI.Keybindings               (handleEvent)
import           UI.Status.Main               (statusbar)
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

getLabelForComposeState :: Mode -> Widget Name
getLabelForComposeState GatherHeadersFrom = txt "From:"
getLabelForComposeState GatherHeadersTo = txt "To:"
getLabelForComposeState _ = txt "Subject:"

-- | event handling for composing e-mail
-- | asks interactively for headers before composing the e-mail
interactiveGatherHeaders :: AppState
                         -> Event
                         -> T.EventM Name (T.Next AppState)
interactiveGatherHeaders s e = case view asAppMode s of
  GatherHeadersFrom ->
            handleEvent
                (view (asConfig . confComposeView . cvFromKeybindings) s)
                interactiveGatherHeadersDefault
                s
                e
  GatherHeadersTo ->
            handleEvent
                (view (asConfig . confComposeView . cvToKeybindings) s)
                interactiveGatherHeadersDefault
                s
                e
  _ ->
            handleEvent
                (view (asConfig . confComposeView . cvSubjectKeybindings) s)
                interactiveGatherHeadersDefault
                s
                e

interactiveGatherHeadersDefault :: AppState -> Event -> T.EventM Name (T.Next AppState)
interactiveGatherHeadersDefault s ev =
    M.continue =<<
    T.handleEventLensed s (asCompose . focusedLens s) E.handleEditorEvent ev

focusedLens :: AppState -> Lens' Compose (E.Editor Text Name)
focusedLens s = case view asAppMode s of
  GatherHeadersFrom -> cFrom
  GatherHeadersTo -> cTo
  _ -> cSubject
