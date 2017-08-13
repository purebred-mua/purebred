#!/usr/bin/runghc
-- This is an example configuration with Keybindings for mutt
import           UI.App               (initialState, theApp)

import           Brick.Main           (halt)
import qualified Brick.Main           as M
import           Config.Main          (defaultConfig)
import           Control.Lens.Lens    ((&))
import           Control.Lens.Setter  (set)
import           Control.Monad        (void)
import qualified Graphics.Vty         as V
import           Storage.Notmuch      (getDatabasePath)
import           UI.Index.Keybindings
import Types

myIndexKeybindings :: [Keybinding]
myIndexKeybindings =
    [ Keybinding "Quits the application" (V.EvKey (V.KChar 'q') []) halt
    , Keybinding
          "Manipulate the notmuch database query"
          (V.EvKey (V.KChar '/') [])
          focusSearch
    , Keybinding "display an e-mail" (V.EvKey V.KEnter []) displayMail
    , Keybinding "mail index down" (V.EvKey V.KDown []) mailIndexDown
    , Keybinding "mail index down" (V.EvKey (V.KChar 'j') []) mailIndexDown
    , Keybinding "mail index up" (V.EvKey V.KUp []) mailIndexUp
    , Keybinding "mail index up" (V.EvKey (V.KChar 'k') []) mailIndexUp
    , Keybinding "Switch between editor and main" (V.EvKey (V.KChar '\t') []) toggleComposeEditorAndMain
    , Keybinding "compose new mail" (V.EvKey (V.KChar 'm') []) composeMail]

myMailKeybindings :: [Keybinding]
myMailKeybindings =
    [ Keybinding
          "Return to list of mails"
          (V.EvKey (V.KChar 'q') [])
          (\s ->
                M.continue $ set asAppMode Main $ s)]

main :: IO ()
main = do
    cfg <- defaultConfig =<< getDatabasePath
    let cfg' =
            set (confIndexView . ivKeybindings) myIndexKeybindings cfg &
            set (confMailView . mvKeybindings) myMailKeybindings
    s <- initialState cfg'
    void $ M.defaultMain (theApp s) s
