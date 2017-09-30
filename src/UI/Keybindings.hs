{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module UI.Keybindings where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Control.Lens.Getter (view)
import Control.Lens.At (at)
import Control.Lens.Setter (set)
import Control.Lens.Lens (Lens')
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Graphics.Vty.Input.Events (Event)
import Prelude hiding (readFile, unlines)
import UI.Index.Keybindings
       (updateStateWithParsedMail, updateReadState)
import Storage.Notmuch (removeTag)
import Types


-- | try look up a registered keybinding in the configuration and if that fails
--   redirect the event to a core widget selected for the current application mode
handleKeyboardEvent :: AppState
                    -> T.BrickEvent Name e
                    -> T.EventM Name (T.Next AppState)
handleKeyboardEvent s (T.VtyEvent ev) =
    let mode = view asAppMode s
        kbs = fromMaybe [] $ view (asConfig . confKeybindings . at mode) s
    in case lookupKeybinding ev kbs of
           Just kb -> view kbAction kb s
           Nothing -> fallbackKeybindingHandler mode s ev
handleKeyboardEvent s _ = M.continue s


fallbackKeybindingHandler :: Mode
                          -> AppState
                          -> Event
                          -> T.EventM Name (T.Next AppState)
fallbackKeybindingHandler BrowseMail s ev =
    M.continue =<<
    T.handleEventLensed s (asMailIndex . miListOfMails) L.handleListEvent ev
fallbackKeybindingHandler SearchMail s ev =
    M.continue =<<
    T.handleEventLensed s (asMailIndex . miSearchEditor) E.handleEditorEvent ev
fallbackKeybindingHandler GatherHeaders s ev =
    M.continue =<<
    T.handleEventLensed s (asCompose . focusedLens s) E.handleEditorEvent ev
fallbackKeybindingHandler ComposeEditor s _ = M.continue s
fallbackKeybindingHandler ViewMail s ev =  do
            l' <- L.handleListEvent ev (view (asMailIndex . miListOfMails) s)
            s' <- liftIO $ updateStateWithParsedMail (set (asMailIndex . miListOfMails) l' s)
                  >>= updateReadState removeTag
            M.continue s'

lookupKeybinding :: Event -> [Keybinding] -> Maybe Keybinding
lookupKeybinding e = find (\x -> view kbEvent x == e)

cancelToMain :: AppState -> T.EventM Name (T.Next AppState)
cancelToMain s = M.continue $ set asAppMode BrowseMail s

initialCompose :: Compose
initialCompose =
    Compose
        Nothing
        AskFrom
        (E.editor GatherHeadersFrom Nothing "")
        (E.editor GatherHeadersTo Nothing "")
        (E.editor GatherHeadersSubject Nothing "")

focusedLens :: AppState -> Lens' Compose (E.Editor Text Name)
focusedLens s = case view (asCompose . cFocus) s of
  AskFrom -> cFrom
  AskTo -> cTo
  AskSubject -> cSubject
