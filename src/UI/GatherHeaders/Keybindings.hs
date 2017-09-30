module UI.GatherHeaders.Keybindings where

import qualified Graphics.Vty as V
import Control.Monad.IO.Class (liftIO)
import Brick.Main (continue, suspendAndResume)
import qualified Brick.Types as T
import UI.Keybindings (cancelToMain)
import Data.Semigroup ((<>))
import System.Exit (ExitCode(..))
import System.IO.Temp (emptySystemTempFile)
import System.Process (system)
import Control.Lens (view, set, (&), (?~))
import Types

interactiveGatherHeadersKeybindings :: [Keybinding]
interactiveGatherHeadersKeybindings =
    [ Keybinding "Return to list of mails" (V.EvKey V.KEsc []) cancelToMain
    , Keybinding "apply" (V.EvKey V.KEnter []) applyAndNextFocus]

applyAndNextFocus :: AppState -> T.EventM Name (T.Next AppState)
applyAndNextFocus s =
    if view (asCompose . cFocus) s == AskSubject
        then suspendAndResume $ liftIO $ invokeEditor s
        else continue $ nextFocus s

invokeEditor :: AppState -> IO AppState
invokeEditor s = do
  let editor = view (asConfig . confEditor) s
  tmpfile <- emptySystemTempFile "purebred.tmp"
  status <- system (editor <> " " <> tmpfile)
  case status of
    ExitFailure _ -> pure $ set asAppMode BrowseMail s -- ^ show error XXX
    ExitSuccess -> pure $ set asAppMode ComposeEditor s & asCompose . cTmpFile ?~ tmpfile -- ^ go to compose editor

nextFocus :: AppState -> AppState
nextFocus s =
    case view (asCompose . cFocus) s of
        AskFrom -> set (asCompose . cFocus) AskTo s
        AskTo -> set (asCompose . cFocus) AskSubject s
        AskSubject -> set (asCompose . cFocus) AskTo s  -- no-op, since we spawn the editor before we draw the widget
