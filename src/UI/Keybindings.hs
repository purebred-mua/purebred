{-# LANGUAGE OverloadedStrings #-}
module UI.Keybindings where

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Edit as E
import Control.Lens.Getter (view)
import Control.Lens.Setter (set)
import Data.List (find)
import Graphics.Vty.Input.Events (Event)
import Prelude hiding (readFile, unlines)
import Types


-- | A generic event handler using Keybindings by default if available
handleEvent
    :: [Keybinding]  -- ^ Keybindings to lookup
    -> (AppState -> Event -> T.EventM Name (T.Next AppState))  -- ^ default handler if no keybinding matches
    -> AppState
    -> T.BrickEvent Name e
    -> T.EventM Name (T.Next AppState)
handleEvent kbs def s (T.VtyEvent ev) =
    case lookupKeybinding ev kbs of
        Just kb -> view kbAction kb s
        Nothing -> def s ev
handleEvent _ _ s _ = M.continue s

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
