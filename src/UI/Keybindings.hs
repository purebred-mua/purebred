{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
module UI.Keybindings where

import qualified Brick.Types as Brick
import qualified Brick.Main as Brick
import qualified Brick.Focus as Brick
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Graphics.Vty (Event (..))
import Control.Lens ((&), view, set)
import Data.List (find)
import Prelude hiding (readFile, unlines)
import Data.Proxy
import Types

lookupKeybinding :: Event -> [Keybinding ctx a] -> Maybe (Keybinding ctx a)
lookupKeybinding e = find (\x -> view kbEvent x == e)

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
  keybindingsL _ = asConfig . confIndexView . ivBrowseMailsKeybindings
  fallbackHandler _ s e = Brick.continue =<< Brick.handleEventLensed s (asMailIndex . miListOfMails) L.handleListEvent e

instance EventHandler 'BrowseThreads where
  keybindingsL _ = asConfig . confIndexView . ivBrowseThreadsKeybindings
  fallbackHandler _ s e = Brick.continue =<< Brick.handleEventLensed s (asMailIndex . miListOfThreads) L.handleListEvent e

instance EventHandler 'SearchThreads where
  keybindingsL _ = asConfig . confIndexView . ivSearchThreadsKeybindings
  fallbackHandler _ s e = Brick.continue =<< Brick.handleEventLensed s (asMailIndex . miSearchThreadsEditor) E.handleEditorEvent e

instance EventHandler 'ManageMailTags where
  keybindingsL _ = asConfig . confIndexView . ivManageMailTagsKeybindings
  fallbackHandler _ s e = Brick.continue =<< Brick.handleEventLensed s (asMailIndex . miMailTagsEditor) E.handleEditorEvent e

instance EventHandler 'ManageThreadTags where
  keybindingsL _ = asConfig . confIndexView . ivManageThreadTagsKeybindings
  fallbackHandler _ s e = Brick.continue =<< Brick.handleEventLensed s (asMailIndex . miThreadTagsEditor) E.handleEditorEvent e

instance EventHandler 'ViewMail where
  keybindingsL _ = asConfig . confMailView . mvKeybindings
  fallbackHandler _ s e = maybe
                          (Brick.continue s)
                          (\kb -> view (kbAction . aAction) kb s)
                          (lookupKeybinding e $ view (asConfig . confMailView . mvIndexKeybindings) s)

instance EventHandler 'ComposeEditor where
  keybindingsL _ = asConfig . confComposeView . cvKeybindings
  fallbackHandler _ s e = Brick.continue
                          =<< maybe (pure s)
                          (\n -> case n of
                              ListOfAttachments -> Brick.handleEventLensed s (asCompose . cAttachments) L.handleListEvent e
                              _ -> Brick.handleEventLensed s (cFocusedEditorL n) E.handleEditorEvent e)
                          (Brick.focusGetCurrent (view (asCompose . cFocusFields) s))

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
                      Just kb -> s & view (kbAction . aAction) kb . set asError Nothing
                      Nothing -> fallbackHandler m s e
