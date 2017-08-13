{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module UI.GatherHeaders.Main where

import qualified Brick.Main                   as M
import           Brick.Types                  (Padding (..), Widget)
import qualified Brick.Types                  as T
import           Brick.Widgets.Core           (padRight, txt, vBox, vLimit,
                                               (<+>))
import qualified Brick.Widgets.Edit           as E
import           Control.Lens.Getter          ((^.))
import           Control.Lens.Lens            (Lens', (&))
import           Control.Lens.Setter          ((.~), (?~))
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
    inputPrefix = padRight (Pad 1) $ getLabelForComposeState (s ^. asCompose ^. cFocus)
    ui = vBox [renderMailList s, statusbar s, inputPrefix <+> vLimit 1 inputBox]

focusedEditor :: AppState -> E.Editor T.Text Name
focusedEditor s =
    case s ^. asCompose ^. cFocus of
        AskFrom -> s ^. asCompose ^. cFrom
        AskTo -> s ^. asCompose ^. cTo
        AskSubject -> s ^. asCompose ^. cSubject

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
            if s ^. asCompose ^. cFocus == AskSubject
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
focusedLens s = case s ^. asCompose ^. cFocus of
  AskFrom -> cFrom
  AskTo -> cTo
  AskSubject -> cSubject

nextFocus :: AppState -> AppState
nextFocus s =
    case s ^. asCompose ^. cFocus of
        AskFrom -> s & asCompose . cFocus .~ AskTo
        AskTo -> s & asCompose . cFocus .~ AskSubject
        AskSubject -> s & asCompose . cFocus .~ AskTo  -- no-op, since we spawn the editor before we draw the widget

invokeEditor :: AppState -> IO (AppState)
invokeEditor s = do
  mEnv <- lookupEnv "EDITOR"
  let editor = fromMaybe "vi" mEnv
  tmpfile <- emptySystemTempFile "purebred.tmp"
  status <- system (editor <> " " <> tmpfile)
  case status of
    ExitFailure _ -> pure $ s & asAppMode .~ Main -- ^ show error XXX
    ExitSuccess -> pure $ s & asAppMode .~ ComposeEditor & asCompose . cTmpFile ?~ tmpfile -- ^ go to compose editor
