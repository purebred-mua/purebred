{-# LANGUAGE OverloadedStrings #-}
module UI.Keybindings where

import qualified Brick.Main as M
import qualified Brick.Types as T
import Control.Lens.Getter (view)
import Data.List (find)
import Graphics.Vty.Input.Events (Event)
import Prelude hiding (readFile, unlines)
import Types


-- | A generic event handler using Keybindings by default if available
handleEvent
    :: [Keybinding a]  -- ^ Keybindings to lookup
    -> (AppState -> Event -> T.EventM Name (T.Next AppState))  -- ^ default handler if no keybinding matches
    -> AppState
    -> Event
    -> T.EventM Name (T.Next AppState)
handleEvent kbs def s ev =
    case lookupKeybinding ev kbs of
        Just kb -> view (kbAction . aAction) kb s
        Nothing -> def s ev

lookupKeybinding :: Event -> [Keybinding a] -> Maybe (Keybinding a)
lookupKeybinding e = find (\x -> view kbEvent x == e)
