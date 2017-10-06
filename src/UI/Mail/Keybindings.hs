module UI.Mail.Keybindings where

import qualified Brick.Types as T
import qualified Graphics.Vty as V
import UI.Actions
import Types

{-# ANN module ("HLint: ignore Avoid lambda" :: String) #-}

displayMailKeybindings :: [Keybinding (T.Widget Name)]
displayMailKeybindings =
    [ Keybinding (V.EvKey V.KBS []) scrollUp
    , Keybinding (V.EvKey (V.KChar ' ') []) scrollDown
    , Keybinding (V.EvKey (V.KChar 'h') []) toggleHeaders
    , Keybinding (V.EvKey V.KEsc []) backToIndex
    ]
