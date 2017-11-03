{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Example configuration, which uses more mutt-alike keybindings
-}
import Purebred
import Data.List (union)

myBrowseThreadsKbs :: [Keybinding 'BrowseThreads (Next AppState)]
myBrowseThreadsKbs =
  [ Keybinding (EvKey (KChar 'q') []) quit
  ]

myBrowseMailKeybindings :: [Keybinding 'BrowseMail (Next AppState)]
myBrowseMailKeybindings =
    [ Keybinding (EvKey (KChar 'q') []) (focus `chain'` (focus :: Action 'BrowseThreads AppState) `chain` continue)
    , Keybinding (EvKey (KChar '/') []) (focus `chain` continue)
    , Keybinding (EvKey KEnter []) (displayMail `chain` continue)
    , Keybinding (EvKey KDown []) (mailIndexDown `chain` continue)
    , Keybinding (EvKey (KChar 'j') []) (mailIndexDown `chain` continue)
    , Keybinding (EvKey KUp []) (mailIndexUp `chain` continue)
    , Keybinding (EvKey (KChar 'k') []) (mailIndexUp `chain` continue)
    , Keybinding (EvKey (KChar 'a') []) ((removeTags ["inbox"] `chain` addTags ["archive"]) `chain` continue)
    ]

myMailKeybindings :: [Keybinding 'ViewMail (Next AppState)]
myMailKeybindings =
    [ Keybinding (EvKey (KChar 'q') []) (noop `chain'` (focus :: Action 'BrowseMail AppState) `chain` continue)
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
    over (confIndexView . ivBrowseThreadsKeybindings) (`union` myBrowseThreadsKbs)
    . over (confIndexView . ivBrowseMailsKeybindings) (`union` myBrowseMailKeybindings)
    . over (confMailView . mvKeybindings) (`union` myMailKeybindings)
    . over (confMailView . mvIndexKeybindings) (`union` myDisplayIndexKeybindings)
