module UI.Mail.Keybindings where

import Brick.Widgets.List (List)
import qualified Brick.Types as T
import qualified Graphics.Vty as V
import UI.Actions
import Types

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

displayMailKeybindings :: [Keybinding (T.Widget Name) (T.Next AppState)]
displayMailKeybindings =
    [ Keybinding (V.EvKey V.KBS []) (scrollUp `chain` continue)
    , Keybinding (V.EvKey (V.KChar ' ') []) (scrollDown `chain` continue)
    , Keybinding (V.EvKey (V.KChar 'h') []) (toggleHeaders `chain` continue)
    , Keybinding (V.EvKey V.KEsc []) (backToIndex `chain` continue)
    ]

displayIndexKeybindings :: [Keybinding (List Name NotmuchMail) (T.Next AppState)]
displayIndexKeybindings = [
  Keybinding (V.EvKey V.KDown []) (mailIndexDown `chain` displayMail `chain` continue)
  , Keybinding (V.EvKey V.KUp []) (mailIndexUp `chain` displayMail `chain` continue)
  ]
