{-# LANGUAGE DataKinds #-}

module UI.Mail.Keybindings where

import qualified Brick.Types as Brick
import qualified Graphics.Vty as V
import UI.Actions
import Types

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

displayMailKeybindings :: [Keybinding 'ViewMail 'ScrollingMailView (Brick.Next AppState)]
displayMailKeybindings =
    [ Keybinding (V.EvKey V.KEsc []) (noop `chain'` (focus :: Action 'ViewMail 'ListOfMails AppState) `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'q') []) (noop `chain'` (focus :: Action 'ViewMail 'ListOfMails AppState) `chain` continue)
    , Keybinding (V.EvKey V.KBS []) (scrollUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar ' ') []) (scrollDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'h') []) (toggleHeaders `chain` continue)
    , Keybinding (V.EvKey (V.KChar '`') []) (noop `chain'` (focus :: Action 'ViewMail 'ManageMailTagsEditor AppState) `chain` continue)
    ]

displayIndexKeybindings :: [Keybinding 'ViewMail 'ListOfMails (Brick.Next AppState)]
displayIndexKeybindings =
    [ Keybinding (V.EvKey V.KDown []) (listDown `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey V.KUp []) (listUp `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'j') []) (listDown `chain'` displayMail `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'k') []) (listUp `chain'` displayMail `chain` continue)
    ]
