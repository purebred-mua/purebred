module UI.Index.Keybindings where

import Brick.Types (Next)
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Data.Text (Text)
import qualified Graphics.Vty as V
import UI.Actions
import Types

-- | Default Keybindings
indexKeybindings :: [Keybinding (L.List Name NotmuchMail) (Next AppState)]
indexKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) quit
    , Keybinding (V.EvKey (V.KChar ':') []) (focusSearch `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (displayMail `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (mailIndexDown `chain` continue)
    , Keybinding (V.EvKey V.KUp []) (mailIndexUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar '\t') []) (switchComposeEditor `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'm') []) (composeMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'r') []) (replyMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 't') []) (setUnread `chain` continue)
    ]

indexsearchKeybindings :: [Keybinding (E.Editor Text Name) (Next AppState)]
indexsearchKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (backToIndex `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (applySearchTerms `chain` continue)
    ]
