{-
Example configuration, which uses more mutt-alike keybindings
-}
import Purebred
import Data.List (union)

myIndexKeybindings :: [Keybinding]
myIndexKeybindings =
    [ Keybinding "Quits the application" (EvKey (KChar 'q') []) halt
    , Keybinding
          "Manipulate the notmuch database query"
          (EvKey (KChar '/') [])
          focusSearch
    , Keybinding "display an e-mail" (EvKey KEnter []) displayMail
    , Keybinding "mail index down" (EvKey KDown []) mailIndexDown
    , Keybinding "mail index down" (EvKey (KChar 'j') []) mailIndexDown
    , Keybinding "mail index up" (EvKey KUp []) mailIndexUp
    , Keybinding "mail index up" (EvKey (KChar 'k') []) mailIndexUp
    , Keybinding "Switch between editor and main" (EvKey (KChar '\t') []) toggleComposeEditorAndMain
    , Keybinding "compose new mail" (EvKey (KChar 'm') []) composeMail]

myMailKeybindings :: [Keybinding]
myMailKeybindings =
    [ Keybinding
          "Return to list of mails"
          (EvKey (KChar 'q') [])
          (\s ->
                continue $ set asAppMode Main $ s)]

main :: IO ()
main = purebred $ tweak defaultConfig where
  tweak =
    over (confIndexView . ivKeybindings) (`union` myIndexKeybindings)
    . over (confMailView . mvKeybindings) (`union` myMailKeybindings)
