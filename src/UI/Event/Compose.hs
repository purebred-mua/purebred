{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
-- | event handling for composing e-mail
module UI.Event.Compose where

import qualified Brick.Main                as M
import qualified Brick.Types               as T
import qualified Brick.Widgets.Edit        as E
import           Control.Lens.Getter       ((^.))
import           Control.Lens.Lens         (Lens', (&))
import           Control.Lens.Setter       ((.~), (?~))
import           Control.Monad.IO.Class    (liftIO)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text)
import           Graphics.Vty              (Event (..), Key (..))
import           System.Environment        (lookupEnv)
import           System.Exit               (ExitCode (..))
import           System.IO.Temp            (emptySystemTempFile)
import           System.Process            (system)
import           UI.Keybindings            (composeEditorKeybindings,
                                            handleEvent,
                                            interactiveGatherHeadersKeybindings)
import           UI.Types

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

-- | the editor which shows header fields and attachments
composeEditor :: AppState
              -> T.BrickEvent Name e
              -> T.EventM Name (T.Next AppState)
composeEditor s e = handleEvent composeEditorKeybindings (\s' _ -> M.continue s') s e

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
