{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module UI.Keybindings where

import Control.Monad ((<=<))
import qualified Brick.Types as Brick
import qualified Brick.Main as Brick
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import Graphics.Vty (Event (..))
import Control.Lens ((&), view, set)
import Data.List (find)
import Prelude hiding (readFile, unlines)
import Types

lookupKeybinding :: Event -> [Keybinding v ctx a] -> Maybe (Keybinding v ctx a)
lookupKeybinding e = find (\x -> view kbEvent x == e)

data EventHandler v m = EventHandler
  (forall f. Functor f
    => ([Keybinding v m (Brick.Next AppState)] -> f [Keybinding v m (Brick.Next AppState)])
    -> AppState -> f AppState) -- lens to keybindings
  (AppState -> Event -> Brick.EventM Name (Brick.Next AppState)) -- fallback handler

dispatch :: EventHandler v m -> AppState -> Event -> Brick.EventM Name (Brick.Next AppState)
dispatch (EventHandler l fallback) s ev =
  case lookupKeybinding ev (view l s) of
    Just kb -> s & view (kbAction . aAction) kb . set asError Nothing
    Nothing -> fallback s ev

-- | Do nothing.  It might be worthwhile to enhance this to display
-- a message like "no binding for key <blah>".
--
nullEventHandler :: EventHandler v m
nullEventHandler = EventHandler (\f s -> s <$ f []) (const . Brick.continue)


eventHandlerListOfMails :: EventHandler 'Mails 'ListOfMails
eventHandlerListOfMails = EventHandler
  (asConfig . confIndexView . ivBrowseMailsKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asMailIndex . miListOfMails) L.handleListEvent)

eventHandlerListOfThreads :: EventHandler 'Threads 'ListOfThreads
eventHandlerListOfThreads = EventHandler
  (asConfig . confIndexView . ivBrowseThreadsKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asMailIndex . miListOfThreads) L.handleListEvent)

eventHandlerSearchThreadsEditor :: EventHandler 'Threads 'SearchThreadsEditor
eventHandlerSearchThreadsEditor = EventHandler
  (asConfig . confIndexView . ivSearchThreadsKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asMailIndex . miSearchThreadsEditor) E.handleEditorEvent)

eventHandlerManageMailTagsEditor :: EventHandler 'Mails 'ManageMailTagsEditor
eventHandlerManageMailTagsEditor = EventHandler
  (asConfig . confIndexView . ivManageMailTagsKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asMailIndex . miMailTagsEditor) E.handleEditorEvent)

eventHandlerViewMailManageMailTagsEditor :: EventHandler 'ViewMail 'ManageMailTagsEditor
eventHandlerViewMailManageMailTagsEditor = EventHandler
  (asConfig . confMailView . mvManageMailTagsKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asMailIndex . miMailTagsEditor) E.handleEditorEvent)

eventHandlerManageThreadTagsEditor :: EventHandler 'Threads 'ManageThreadTagsEditor
eventHandlerManageThreadTagsEditor = EventHandler
  (asConfig . confIndexView . ivManageThreadTagsKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asMailIndex . miThreadTagsEditor) E.handleEditorEvent)

eventHandlerScrollingMailView :: EventHandler 'ViewMail 'ScrollingMailView
eventHandlerScrollingMailView = EventHandler
  (asConfig . confMailView . mvKeybindings)
  (const . Brick.continue)

eventHandlerScrollingHelpView :: EventHandler 'Help 'ScrollingHelpView
eventHandlerScrollingHelpView = EventHandler
  (asConfig . confHelpView . hvKeybindings)
  (const . Brick.continue)

eventHandlerThreadComposeFrom :: EventHandler 'Threads 'ComposeFrom
eventHandlerThreadComposeFrom = EventHandler
  (asConfig . confIndexView . ivFromKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asCompose . cFrom) E.handleEditorEvent)

eventHandlerThreadComposeTo :: EventHandler 'Threads 'ComposeTo
eventHandlerThreadComposeTo = EventHandler
  (asConfig . confIndexView . ivToKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asCompose . cTo) E.handleEditorEvent)

eventHandlerThreadComposeSubject :: EventHandler 'Threads 'ComposeSubject
eventHandlerThreadComposeSubject = EventHandler
  (asConfig . confIndexView . ivSubjectKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asCompose . cSubject) E.handleEditorEvent)

eventHandlerComposeFrom :: EventHandler 'ComposeView 'ComposeFrom
eventHandlerComposeFrom = EventHandler
  (asConfig . confComposeView . cvFromKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asCompose . cFrom) E.handleEditorEvent)

eventHandlerComposeTo :: EventHandler 'ComposeView 'ComposeTo
eventHandlerComposeTo = EventHandler
  (asConfig . confComposeView . cvToKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asCompose . cTo) E.handleEditorEvent)

eventHandlerComposeSubject :: EventHandler 'ComposeView 'ComposeSubject
eventHandlerComposeSubject = EventHandler
  (asConfig . confComposeView . cvSubjectKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asCompose . cSubject) E.handleEditorEvent)

eventHandlerComposeListOfAttachments :: EventHandler 'ComposeView 'ListOfAttachments
eventHandlerComposeListOfAttachments = EventHandler
  (asConfig . confComposeView . cvListOfAttachmentsKeybindings)
  (\s -> Brick.continue <=< Brick.handleEventLensed s (asCompose . cAttachments) L.handleListEvent)
