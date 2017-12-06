{-# LANGUAGE DataKinds #-}

module UI.Mail.Keybindings where

import qualified Brick.Types as Brick
import qualified Graphics.Vty as V
import UI.Actions
import Types

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

displayMailKeybindings :: [Keybinding 'ViewMail (Brick.Next AppState)]
displayMailKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'BrowseMail AppState) `chain` continue)
    , Keybinding (V.EvKey V.KBS []) (scrollUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar ' ') []) (scrollDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'h') []) (toggleHeaders `chain` continue)
    , Keybinding (V.EvKey (V.KChar '`') []) (noop `chain'` (focus :: Action 'ManageMailTags AppState) `chain` continue)
    ]

displayIndexKeybindings :: [Keybinding 'BrowseMail (Brick.Next AppState)]
displayIndexKeybindings =
    [ Keybinding (V.EvKey V.KDown []) (listDown `chain` displayMail `chain` continue)
    , Keybinding (V.EvKey V.KUp []) (listUp `chain` displayMail `chain` continue)
    ]
