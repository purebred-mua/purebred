module UI.Views
  ( indexView
  , listOfMailsView
  , mailView
  , composeView
  , helpView
  , filebrowserView
  , swapWidget
  , focusedViewWidgets
  , focusedViewWidget
  , focusedViewName
  , focusedView
  , toggleLastVisibleWidget
  , resetView
  ) where

import Data.Vector (fromList)
import Data.Maybe (fromMaybe)
import Prelude hiding (splitAt)

import Control.Lens
  ((&), _Just, ix, at, set, lastOf, view, filtered, over, toListOf,
   traversed)
import Brick.Focus (focusGetCurrent)
import Types


focusedViewWidgets :: AppState -> [Name]
focusedViewWidgets s =
    let defaultV = view (asConfig . confDefaultView) s
        focused = fromMaybe defaultV $ focusGetCurrent $ view (asViews . vsFocusedView) s
    in toListOf (asViews . vsViews . at focused . _Just . vWidgets . tileiso . traversed
                 . filtered (\ve -> view veState ve == Visible) . veName) s

focusedViewWidget :: AppState -> Name
focusedViewWidget s = view vFocus (focusedView s)

focusedView :: AppState -> View
focusedView s = let focused = view (asViews . vsViews . at (focusedViewName s)) s
                in fromMaybe indexView focused

-- | Swap the widget with the given name with the current widget shown at the bottom of the UI
toggleLastVisibleWidget :: Name -> AppState -> AppState
toggleLastVisibleWidget n s =
  let fallback = focusedViewWidget s
  in s
     & over (asViews . vsViews . at (focusedViewName s) . _Just . vWidgets) (swapWidget fallback n)
     . set (asViews . vsViews . at (focusedViewName s) . _Just . vFocus) n

swapWidget :: Name -> Name -> Tiles -> Tiles
swapWidget fallback n m =
  let lastWidget = fromMaybe fallback $ lastOf (tileiso . traverse . filtered (\x -> view veState x == Visible) . veName) m
   in m & set (ix lastWidget . veState) Hidden . set (ix n . veState) Visible

focusedViewName :: AppState -> ViewName
focusedViewName s =
    let defaultV = view (asConfig . confDefaultView) s
    in fromMaybe defaultV $ focusGetCurrent $ view (asViews . vsFocusedView) s

resetView :: ViewName -> View -> AppState -> AppState
resetView n = set (asViews . vsViews . ix n)

indexView :: View
indexView =
  View
    { _vWidgets =
        Tiles $ fromList
          [ Tile Visible ListOfThreads
          , Tile Visible StatusBar
          , Tile Visible SearchThreadsEditor
          , Tile Hidden ManageThreadTagsEditor
          , Tile Hidden ComposeFrom
          , Tile Hidden ComposeTo
          , Tile Hidden ComposeSubject
          ]
    , _vFocus = ListOfThreads
    }

listOfMailsView :: View
listOfMailsView =
  View
    { _vWidgets =
        Tiles $ fromList
          [ Tile Visible ListOfMails
          , Tile Visible StatusBar
          , Tile Visible ManageMailTagsEditor
          ]
    , _vFocus = ListOfMails
    }

mailView :: View
mailView =
  View
    { _vWidgets =
        Tiles $ fromList
          [ Tile Visible ListOfMails
          , Tile Visible StatusBar
          , Tile Visible ScrollingMailView
          , Tile Hidden ManageMailTagsEditor
          , Tile Hidden MailListOfAttachments
          , Tile Hidden MailAttachmentOpenWithEditor
          , Tile Hidden MailAttachmentPipeToEditor
          ]
    , _vFocus = ListOfMails
    }

composeView :: View
composeView =
  View
    { _vWidgets =
        Tiles $ fromList
          [ Tile Visible ComposeHeaders
          , Tile Visible ComposeListOfAttachments
          , Tile Visible StatusBar
          , Tile Hidden ComposeFrom
          , Tile Hidden ComposeTo
          , Tile Hidden ComposeSubject
          , Tile Hidden ConfirmDialog
          ]
    , _vFocus = ComposeFrom
    }

helpView :: View
helpView =
  View
    { _vWidgets = Tiles $ fromList [Tile Visible ScrollingHelpView]
    , _vFocus = ScrollingHelpView
    }

filebrowserView :: View
filebrowserView =
  View
    { _vWidgets =
        Tiles $ fromList
          [ Tile Visible ListOfFiles
          , Tile Visible StatusBar
          , Tile Visible ManageFileBrowserSearchPath
          ]
    , _vFocus = ListOfFiles
    }
