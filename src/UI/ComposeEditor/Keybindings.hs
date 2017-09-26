{-# LANGUAGE OverloadedStrings #-}

module UI.ComposeEditor.Keybindings where

import qualified Brick.Main             as M
import qualified Brick.Types            as T
import qualified Brick.Widgets.Edit     as E
import Control.Lens.Fold ((^?!))
import Control.Lens.Getter (view)
import Control.Lens.Lens ((&))
import Control.Lens.Prism (_Just)
import Control.Lens.Setter (set)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (unlines)
import           Data.Text.Lazy.IO      (readFile)
import qualified Graphics.Vty           as V
import           Network.Mail.Mime      (Address (..), renderSendMail,
                                         simpleMail')
import           Prelude                hiding (readFile, unlines)
import           UI.Keybindings         (cancelToMain, initialCompose)
import Types
       (AppState, Keybinding(..), Mode(..), Name(..),
        asAppMode, asCompose, cFrom, cSubject, cTmpFile, cTo)


composeEditorKeybindings :: [Keybinding]
composeEditorKeybindings =
    [ Keybinding "Toggle index view" (V.EvKey (V.KChar '\t') []) cancelToMain
    , Keybinding "Send e-mail" (V.EvKey (V.KChar 'y') []) sendMail
    , Keybinding "Cancel compose" (V.EvKey V.KEsc []) (M.continue . resetCompose)
    ]

sendMail :: AppState -> T.EventM Name (T.Next AppState)
sendMail s = do
    -- XXX if something has removed the tmpfile for whatever reason we go b00m :(
    body <- liftIO $ readFile (view (asCompose . cTmpFile) s ^?! _Just)
    let to =
            Address
                Nothing
                (unlines $ E.getEditContents $ view (asCompose . cTo) s)
    let from =
            Address
                Nothing
                (unlines $ E.getEditContents $ view (asCompose . cFrom) s)
    let m =
            simpleMail'
                to
                from
                (unlines $ E.getEditContents $ view (asCompose . cSubject) s)
                body
    liftIO $ renderSendMail m
    M.continue $ set asCompose initialCompose s & set asAppMode Main

resetCompose :: AppState -> AppState
resetCompose s = set asCompose initialCompose s & set asAppMode Main
