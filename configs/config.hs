{-
Example configuration, which uses more mutt-alike keybindings
-}
import Purebred
import Data.List (union)

myIndexKeybindings :: [Keybinding (List Name NotmuchMail)]
myIndexKeybindings =
    [ Keybinding (EvKey (KChar 'q') []) haltApp
    , Keybinding
          (EvKey (KChar '/') [])
          focusSearch
    , Keybinding (EvKey KEnter []) displayMail
    , Keybinding (EvKey KDown []) mailIndexDown
    , Keybinding (EvKey (KChar 'j') []) mailIndexDown
    , Keybinding (EvKey KUp []) mailIndexUp
    , Keybinding (EvKey (KChar 'k') []) mailIndexUp
    , Keybinding (EvKey (KChar '\t') []) switchComposeEditor
    , Keybinding (EvKey (KChar 'm') []) composeMail]

myMailKeybindings :: [Keybinding a]
myMailKeybindings =
    [ Keybinding (EvKey (KChar 'q') []) backToIndex
    ]

main :: IO ()
main = purebred $ tweak defaultConfig where
  tweak =
    over (confIndexView . ivKeybindings) (`union` myIndexKeybindings)
    . over (confMailView . mvKeybindings) (`union` myMailKeybindings)
