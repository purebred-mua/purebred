{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Example configuration, which uses more mutt-alike keybindings
-}
import Purebred
import Data.List (union)

myIndexKeybindings :: [Keybinding 'BrowseMail (Next AppState)]
myIndexKeybindings =
    [ Keybinding (EvKey (KChar 'q') []) quit
    , Keybinding (EvKey (KChar '/') []) (focus `chain` continue)
    , Keybinding (EvKey KEnter []) (displayMail `chain` continue)
    , Keybinding (EvKey KDown []) (mailIndexDown `chain` continue)
    , Keybinding (EvKey (KChar 'j') []) (mailIndexDown `chain` continue)
    , Keybinding (EvKey KUp []) (mailIndexUp `chain` continue)
    , Keybinding (EvKey (KChar 'k') []) (mailIndexUp `chain` continue)
    , Keybinding (EvKey (KChar '\t') []) (switchComposeEditor `chain` continue)
    , Keybinding (EvKey (KChar 'a') []) ((removeTags ["inbox"] `chain` addTags ["archive"]) `chain` reloadMails `chain` continue)
    , Keybinding (EvKey (KChar 'm') []) (composeMail `chain` continue)]

myMailKeybindings :: [Keybinding 'ViewMail (Next AppState)]
myMailKeybindings =
    [ Keybinding (EvKey (KChar 'q') []) (backToIndex `chain` continue)
    , Keybinding (EvKey (KChar 'a') []) ((removeTags ["inbox"] `chain` addTags ["archive"])
                                         `chain'` (mailIndexDown :: Action 'BrowseMail AppState)
                                         `chain` displayMail `chain` continue)
    ]

myDisplayIndexKeybindings :: [Keybinding 'BrowseMail (Next AppState)]
myDisplayIndexKeybindings =
    [ Keybinding (EvKey (KChar 'j') []) (mailIndexDown `chain` displayMail `chain` continue)
    , Keybinding (EvKey (KChar 'k') []) (mailIndexUp `chain` displayMail `chain` continue)
    ]

main :: IO ()
main = purebred $ tweak defaultConfig where
  tweak =
    over (confIndexView . ivKeybindings) (`union` myIndexKeybindings)
    . over (confMailView . mvKeybindings) (`union` myMailKeybindings)
    . over (confMailView . mvIndexKeybindings) (`union` myDisplayIndexKeybindings)
