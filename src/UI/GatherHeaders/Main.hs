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
import           Control.Lens.Lens            (Lens', (&))
import           Control.Lens.Setter          (set, (?~))
import           Control.Monad.IO.Class       (liftIO)
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Graphics.Vty                 (Event (..), Key (..))
import           System.Environment           (lookupEnv)
import           System.Exit                  (ExitCode (..))
import           System.IO.Temp               (emptySystemTempFile)
import           System.Process               (system)
import           UI.Draw.Main                 (editorDrawContent)
import           UI.GatherHeaders.Keybindings (interactiveGatherHeadersKeybindings)
import           UI.Index.Main                (renderMailList)
import           UI.Keybindings               (handleEvent)
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

-- | event handling for composing e-mail
-- | asks interactively for headers before composing the e-mail
interactiveGatherHeaders :: AppState
                         -> T.BrickEvent Name e
                         -> T.EventM Name (T.Next AppState)
interactiveGatherHeaders s e =
    case e of
        (T.VtyEvent (EvKey KEnter [])) ->
            if view (asCompose . cFocus) s == AskSubject
                then M.suspendAndResume $ liftIO $ invokeEditor s
                else M.continue $ nextFocus s
        _ ->
            handleEvent
                interactiveGatherHeadersKeybindings
                interactiveGatherHeadersDefault
                s
                e

interactiveGatherHeadersDefault :: AppState -> Event -> T.EventM Name (T.Next AppState)
interactiveGatherHeadersDefault s ev =
    M.continue =<<
    T.handleEventLensed s (asCompose . focusedLens s) E.handleEditorEvent ev

focusedLens :: AppState -> Lens' Compose (E.Editor Text Name)
focusedLens s = case view (asCompose . cFocus) s of
  AskFrom -> cFrom
  AskTo -> cTo
  AskSubject -> cSubject

nextFocus :: AppState -> AppState
nextFocus s =
    case view (asCompose . cFocus) s of
        AskFrom -> set (asCompose . cFocus) AskTo s
        AskTo -> set (asCompose . cFocus) AskSubject s
        AskSubject -> set (asCompose . cFocus) AskTo s  -- no-op, since we spawn the editor before we draw the widget

invokeEditor :: AppState -> IO (AppState)
invokeEditor s = do
  mEnv <- lookupEnv "EDITOR"
  let editor = fromMaybe "vi" mEnv
  tmpfile <- emptySystemTempFile "purebred.tmp"
  status <- system (editor <> " " <> tmpfile)
  case status of
    ExitFailure _ -> pure $ set asAppMode Main s -- ^ show error XXX
    ExitSuccess -> pure $ set asAppMode ComposeEditor s & asCompose . cTmpFile ?~ tmpfile -- ^ go to compose editor
