{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
module UI.Keybindings where

import qualified Brick.Types as Brick
import qualified Brick.Main as Brick
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Graphics.Vty (Event (..), Key (..))
import Control.Lens.Getter (view)
import Data.List (find)
import Prelude hiding (readFile, unlines)
import Data.Proxy
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

class EventHandler (m :: Mode)  where
    keybindingsL
        :: Functor f
        => Proxy m
        -> ([Keybinding m (Brick.Next AppState)] -> f [Keybinding m (Brick.Next AppState)])
        -> AppState
        -> f AppState
    fallbackHandler :: Proxy m
                    -> AppState
                    -> Event
                    -> Brick.EventM Name (Brick.Next AppState)

instance EventHandler 'BrowseMail where
  keybindingsL _ = asConfig . confIndexView . ivKeybindings
  fallbackHandler _ s e = Brick.continue =<< Brick.handleEventLensed s (asMailIndex . miListOfMails) L.handleListEvent e

instance EventHandler 'SearchMail where
  keybindingsL _ = asConfig . confIndexView . ivSearchKeybindings
  fallbackHandler _ s e = Brick.continue =<< Brick.handleEventLensed s (asMailIndex . miSearchEditor) E.handleEditorEvent e

instance EventHandler 'ManageTags where
  keybindingsL _ = asConfig . confIndexView . ivManageTagsKeybindings
  fallbackHandler _ s e = Brick.continue =<< Brick.handleEventLensed s (asMailIndex . miSearchEditor) E.handleEditorEvent e

instance EventHandler 'ViewMail where
  keybindingsL _ = asConfig . confMailView . mvKeybindings
  fallbackHandler _ s e = maybe
                          (Brick.continue s)
                          (\kb -> view (kbAction . aAction) kb s)
                          (lookupKeybinding e $ view (asConfig . confMailView . mvIndexKeybindings) s)

instance EventHandler 'ComposeEditor where
  keybindingsL _ = asConfig . confComposeView . cvKeybindings
  fallbackHandler _ s _ = Brick.continue s

instance EventHandler 'Help where
  keybindingsL _ = asConfig . confHelpView . hvKeybindings
  fallbackHandler _ s _ = Brick.continue s

instance EventHandler 'GatherHeadersFrom where
  keybindingsL _ = asConfig . confComposeView . cvFromKeybindings
  fallbackHandler _ s e = Brick.continue =<< Brick.handleEventLensed s (asCompose . cFrom) E.handleEditorEvent e

instance EventHandler 'GatherHeadersTo where
  keybindingsL _ = asConfig . confComposeView . cvToKeybindings
  fallbackHandler _ s e = Brick.continue =<< Brick.handleEventLensed s (asCompose . cTo) E.handleEditorEvent e

instance EventHandler 'GatherHeadersSubject where
  keybindingsL _ = asConfig . confComposeView . cvSubjectKeybindings
  fallbackHandler _ s e = Brick.continue =<< Brick.handleEventLensed s (asCompose . cSubject) E.handleEditorEvent e

dispatch :: EventHandler m => Proxy m -> AppState -> Event -> Brick.EventM Name (Brick.Next AppState)
dispatch m s e = let kbs = view (keybindingsL m) s
                 in case lookupKeybinding e kbs of
                      Just kb -> view (kbAction . aAction) kb s
                      Nothing -> fallbackHandler m s e
