module UI.Index.Keybindings where

import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Data.Text (Text)
import qualified Graphics.Vty as V
import UI.Actions
import Types

-- | Default Keybindings
indexKeybindings :: [Keybinding (L.List Name NotmuchMail)]
indexKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) haltApp
    , Keybinding (V.EvKey (V.KChar ':') []) focusSearch
    , Keybinding (V.EvKey V.KEnter []) displayMail
    , Keybinding (V.EvKey V.KDown []) mailIndexDown
    , Keybinding (V.EvKey V.KUp []) mailIndexUp
    , Keybinding (V.EvKey (V.KChar '\t') []) switchComposeEditor
    , Keybinding (V.EvKey (V.KChar 'm') []) composeMail
    , Keybinding (V.EvKey (V.KChar 'r') []) replyMail
    , Keybinding (V.EvKey (V.KChar 't') []) setUnread
    ]

indexsearchKeybindings :: [Keybinding (E.Editor Text Name)]
indexsearchKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) backToIndex
    , Keybinding (V.EvKey V.KEnter []) applySearchTerms
    ]
