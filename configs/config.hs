{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-
Example configuration, which uses more mutt-alike keybindings
-}
import Purebred
import Data.List (union)

myBrowseThreadsKbs :: [Keybinding 'BrowseThreads (Next AppState)]
myBrowseThreadsKbs =
  [ Keybinding (EvKey (KChar 'a') []) (setTags [RemoveTag "inbox", AddTag "archive"] `chain` continue)
  ]

myBrowseMailKeybindings :: [Keybinding 'BrowseMail (Next AppState)]
myBrowseMailKeybindings =
    [ Keybinding (EvKey (KChar 'a') []) (setTags [RemoveTag "inbox", AddTag "archive"] `chain` continue)
    ]

myMailKeybindings :: [Keybinding 'ViewMail (Next AppState)]
myMailKeybindings =
    [ Keybinding (EvKey (KChar 'a') []) (setTags [RemoveTag "inbox", AddTag "archive"] `chain` continue)
    ]

main :: IO ()
main = purebred $ tweak defaultConfig where
  tweak =
    over (confIndexView . ivBrowseThreadsKeybindings) (`union` myBrowseThreadsKbs)
    . over (confIndexView . ivBrowseMailsKeybindings) (`union` myBrowseMailKeybindings)
    . over (confMailView . mvKeybindings) (`union` myMailKeybindings)
