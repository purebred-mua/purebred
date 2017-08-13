{-# LANGUAGE OverloadedStrings #-}
module UI.ComposeEditor.Keybindings where

import qualified Brick.Main             as M
import qualified Brick.Types            as T
import qualified Brick.Widgets.Edit     as E
import           Control.Lens.Fold      ((^?!))
import           Control.Lens.Getter    ((^.))
import           Control.Lens.Lens      ((&))
import           Control.Lens.Prism     (_Just)
import           Control.Lens.Setter    ((.~), set)
import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (unlines)
import           Data.Text.Lazy.IO      (readFile)
import qualified Graphics.Vty           as V
import           Network.Mail.Mime      (Address (..), renderSendMail,
                                         simpleMail')
import           Prelude                hiding (readFile, unlines)
import           UI.Keybindings         (cancelToMain, initialCompose)
import Types
       (ComposeState(..), AppState, Keybinding(..), Mode(..), Name(..),
        asAppMode, asCompose, cFrom, cSubject, cTmpFile, cTo, cFocus)


composeEditorKeybindings :: [Keybinding]
composeEditorKeybindings =
    [ Keybinding "Toggle index view" (V.EvKey (V.KChar '\t') []) cancelToMain
    , Keybinding "Send e-mail" (V.EvKey (V.KChar 'y') []) sendMail
    , Keybinding "Cancel compose" (V.EvKey V.KEsc []) (\s -> M.continue $ resetCompose s)]

sendMail :: AppState -> T.EventM Name (T.Next AppState)
sendMail s = do
    body <- liftIO $ readFile (s ^. asCompose ^. cTmpFile ^?! _Just)  -- XXX if something has removed the tmpfile for whatever reason we go b00m :(
    let to =
            Address
                Nothing
                (unlines $ E.getEditContents $ s ^. asCompose ^. cTo)
    let from =
            Address
                Nothing
                (unlines $ E.getEditContents $ s ^. asCompose ^. cFrom)
    let m =
            simpleMail'
                to
                from
                (unlines $ E.getEditContents $ s ^. asCompose ^. cSubject)
                body
    liftIO $ renderSendMail m
    M.continue $ s & asCompose .~ initialCompose & asAppMode .~ Main

resetCompose :: AppState -> AppState
resetCompose s = s & asCompose .~ initialCompose & asAppMode .~ Main
