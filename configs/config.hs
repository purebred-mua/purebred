{-
Example configuration, which uses more mutt-alike keybindings
-}
import Purebred
import Data.List (union)

myIndexKeybindings :: [Keybinding (List Name NotmuchMail) (Next AppState)]
myIndexKeybindings =
    [ Keybinding (EvKey (KChar 'q') []) quit
    , Keybinding (EvKey (KChar '/') []) (focusSearch `chain` continue)
    , Keybinding (EvKey KEnter []) (displayMail `chain` continue)
    , Keybinding (EvKey KDown []) (mailIndexDown `chain` continue)
    , Keybinding (EvKey (KChar 'j') []) (mailIndexDown `chain` continue)
    , Keybinding (EvKey KUp []) (mailIndexUp `chain` continue)
    , Keybinding (EvKey (KChar 'k') []) (mailIndexUp `chain` continue)
    , Keybinding (EvKey (KChar '\t') []) (switchComposeEditor `chain` continue)
    , Keybinding (EvKey (KChar 'm') []) (composeMail `chain` continue)]

myMailKeybindings :: [Keybinding ctx (Next AppState)]
myMailKeybindings =
    [ Keybinding (EvKey (KChar 'q') []) (backToIndex `chain` continue)
    ]

myDisplayIndexKeybindings :: [Keybinding (List Name NotmuchMail) (Next AppState)]
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
