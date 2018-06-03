{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module UI.Keybindings where

import qualified Brick.Types as Brick
import qualified Brick.Main as Brick
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Graphics.Vty (Event (..))
import Control.Lens ((&), view, set)
import Data.List (find)
import Prelude hiding (readFile, unlines)
import Data.Proxy
import Types

lookupKeybinding :: Event -> [Keybinding v ctx a] -> Maybe (Keybinding v ctx a)
lookupKeybinding e = find (\x -> view kbEvent x == e)

class EventHandler (v :: ViewName) (m :: Name)  where
    keybindingsL
        :: Functor f
        => Proxy v
        -> Proxy m
        -> ([Keybinding v m (Brick.Next AppState)] -> f [Keybinding v m (Brick.Next AppState)])
        -> AppState
        -> f AppState
    fallbackHandler :: Proxy v
                    -> Proxy m
                    -> AppState
                    -> Event
                    -> Brick.EventM Name (Brick.Next AppState)

instance EventHandler 'Mails 'ListOfMails where
  keybindingsL _ _ = asConfig . confIndexView . ivBrowseMailsKeybindings
  fallbackHandler _ _ s e = Brick.continue =<< Brick.handleEventLensed s (asMailIndex . miListOfMails) L.handleListEvent e

instance EventHandler 'Threads 'ListOfThreads where
  keybindingsL _ _ = asConfig . confIndexView . ivBrowseThreadsKeybindings
  fallbackHandler _ _ s e = Brick.continue =<< Brick.handleEventLensed s (asMailIndex . miListOfThreads) L.handleListEvent e

instance EventHandler 'Threads 'SearchThreadsEditor where
  keybindingsL _ _ = asConfig . confIndexView . ivSearchThreadsKeybindings
  fallbackHandler _ _ s e = Brick.continue =<< Brick.handleEventLensed s (asMailIndex . miSearchThreadsEditor) E.handleEditorEvent e

instance EventHandler 'Mails 'ManageMailTagsEditor where
  keybindingsL _ _ = asConfig . confIndexView . ivManageMailTagsKeybindings
  fallbackHandler _ _ s e = Brick.continue =<< Brick.handleEventLensed s (asMailIndex . miMailTagsEditor) E.handleEditorEvent e

instance EventHandler 'Threads 'ManageThreadTagsEditor where
  keybindingsL _ _ = asConfig . confIndexView . ivManageThreadTagsKeybindings
  fallbackHandler _ _ s e = Brick.continue =<< Brick.handleEventLensed s (asMailIndex . miThreadTagsEditor) E.handleEditorEvent e

instance EventHandler 'ViewMail 'ScrollingMailView where
  keybindingsL _ _ = asConfig . confMailView . mvKeybindings
  fallbackHandler _ _ s e = maybe
                          (Brick.continue s)
                          (\kb -> view (kbAction . aAction) kb s)
                          (lookupKeybinding e $ view (asConfig . confIndexView . ivBrowseMailsKeybindings) s)

instance EventHandler 'Help 'ScrollingHelpView where
  keybindingsL _ _ = asConfig . confHelpView . hvKeybindings
  fallbackHandler _ _ s _ = Brick.continue s

instance EventHandler 'ComposeView 'ComposeFrom where
  keybindingsL _ _ = asConfig . confComposeView . cvFromKeybindings
  fallbackHandler _ _ s e = Brick.continue =<< Brick.handleEventLensed s (asCompose . cFrom) E.handleEditorEvent e

instance EventHandler 'ComposeView 'ComposeTo where
  keybindingsL _ _ = asConfig . confComposeView . cvToKeybindings
  fallbackHandler _ _ s e = Brick.continue =<< Brick.handleEventLensed s (asCompose . cTo) E.handleEditorEvent e

instance EventHandler 'ComposeView 'ComposeSubject where
  keybindingsL _ _ = asConfig . confComposeView . cvSubjectKeybindings
  fallbackHandler _ _ s e = Brick.continue =<< Brick.handleEventLensed s (asCompose . cSubject) E.handleEditorEvent e

dispatch
    :: EventHandler v m
    => Proxy v
    -> Proxy m
    -> AppState
    -> Event
    -> Brick.EventM Name (Brick.Next AppState)
dispatch v m s e = let kbs = view (keybindingsL v m) s
                   in case lookupKeybinding e kbs of
                      Just kb -> s & view (kbAction . aAction) kb . set asError Nothing
                      Nothing -> fallbackHandler v m s e
