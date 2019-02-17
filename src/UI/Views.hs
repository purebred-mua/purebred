module UI.Views
  ( indexView
  , listOfMailsView
  , mailView
  , composeView
  , helpView
  , filebrowserView
  , swapWidget
  , focusNext
  , focusedViewWidgets
  , focusedViewWidget
  , focusedViewName
  , focusedView
  , toggleLastVisibleWidget
  , resetView
  , findNextVisibleWidget
  ) where

import Data.Vector (find, fromList, splitAt, findIndex, Vector)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import Prelude hiding (splitAt)

import Control.Lens
  ((&), _Just, ix, at, preview, set, lastOf, view, filtered, over, toListOf,
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

-- | Ugh so what do we do here? We want to determine the next visible view
-- element. We used a circular list, which allows us to search forward until
-- we've re-visit the same position from where we left off again. In order to do
-- the same with a list-type data structure, we split the list and tack on the
-- head so we can be sure that we either find a next visible element or well the
-- currently focused.
focusNext :: AppState -> AppState
focusNext s = let focused = preview (asViews . vsViews . at (focusedViewName s) . _Just . vFocus) s
                  widgets = view (asViews . vsViews . at (focusedViewName s) . _Just . vWidgets . tileiso) s
                  index = maybe 0 (\n -> fromMaybe 0 $ findIndex (\x -> view veName x == n) widgets) focused
              in maybe s (\x -> set (asViews . vsViews . at (focusedViewName s) . _Just . vFocus) (findNextVisibleWidget x index widgets) s) focused

resetView :: ViewName -> View -> AppState -> AppState
resetView n = set (asViews . vsViews . at n . _Just)

findNextVisibleWidget ::
     Name -- ^ default fallback
  -> Int -- ^ current index
  -> Vector Tile
  -> Name
findNextVisibleWidget fallback i v =
  let (head', tail') = splitAt (i + 1) v
  in maybe fallback (view veName) $ find (\x -> view veState x == Visible) (tail' <> head')

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
          ]
    , _vFocus = ListOfMails
    }

composeView :: View
composeView =
  View
    { _vWidgets =
        Tiles $ fromList
          [ Tile Visible ComposeFrom
          , Tile Visible ComposeTo
          , Tile Visible ComposeSubject
          , Tile Visible StatusBar
          , Tile Visible ComposeListOfAttachments
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
