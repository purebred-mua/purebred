{-# LANGUAGE OverloadedStrings #-}
module UI.Keybindings where

import qualified Brick.Types as Brick
import Graphics.Vty (Event (..), Key (..))
import Control.Lens.Getter (view)
import Data.List (find)
import Prelude hiding (readFile, unlines)
import Types
import UI.Actions


-- | A generic event handler using Keybindings by default if available
handleEvent
    :: [Keybinding ctx (Brick.Next AppState)]  -- ^ Keybindings to lookup
    -> (AppState -> Event -> Brick.EventM Name (Brick.Next AppState))  -- ^ default handler if no keybinding matches
    -> AppState
    -> Event
    -> Brick.EventM Name (Brick.Next AppState)
handleEvent kbs def s ev =
    case lookupKeybinding ev kbs of
        Just kb -> view (kbAction . aAction) kb s
        Nothing -> def s ev

lookupKeybinding :: Event -> [Keybinding ctx a] -> Maybe (Keybinding ctx a)
lookupKeybinding e = find (\x -> view kbEvent x == e)

scrollableKeybindings :: Scrollable ctx => [Keybinding ctx (Brick.Next AppState)]
scrollableKeybindings =
    [ Keybinding (EvKey KEsc []) (backToIndex `chain` continue)
    , Keybinding (EvKey KBS []) (scrollUp `chain` continue)
    , Keybinding (EvKey (KChar ' ') []) (scrollDown `chain` continue)
    ]
