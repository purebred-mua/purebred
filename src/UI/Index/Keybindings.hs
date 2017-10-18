{-# LANGUAGE DataKinds #-}

module UI.Index.Keybindings where

import qualified Brick.Types as Brick
import qualified Graphics.Vty as V
import UI.Actions
import Types

-- | Default Keybindings
indexKeybindings :: [Keybinding 'BrowseMail (Brick.Next AppState)]
indexKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) quit
    , Keybinding (V.EvKey (V.KChar ':') []) (focus `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (displayMail `chain` continue)
    , Keybinding (V.EvKey V.KDown []) (mailIndexDown `chain` continue)
    , Keybinding (V.EvKey V.KUp []) (mailIndexUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar '\t') []) (switchComposeEditor `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'm') []) (composeMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'r') []) (replyMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 't') []) (setUnread `chain` continue)
    , Keybinding (V.EvKey (V.KChar '?') []) (viewHelp `chain` continue)
    ]

indexsearchKeybindings :: [Keybinding 'SearchMail (Brick.Next AppState)]
indexsearchKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (backToIndex `chain` continue)
    , Keybinding (V.EvKey V.KEnter []) (done `chain` continue)
    ]
