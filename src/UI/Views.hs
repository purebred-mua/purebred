module UI.Views
       (indexView, listOfMailsView, mailView, composeView, helpView, filebrowserView) where

import Brick.Focus (focusRing)
import Types


indexView :: View
indexView =
    View
    { _vFocus = focusRing [ListOfThreads, SearchThreadsEditor, ManageThreadTagsEditor, ComposeFrom, ComposeTo, ComposeSubject]
    , _vWidgets = [ListOfThreads, StatusBar, SearchThreadsEditor]
    }

listOfMailsView :: View
listOfMailsView =
    View
    { _vFocus = focusRing [ListOfMails, ManageMailTagsEditor]
    , _vWidgets = [ListOfMails, StatusBar]
    }

mailView :: View
mailView =
    View
    { _vFocus = focusRing [ListOfMails, ScrollingMailView, ManageMailTagsEditor]
    , _vWidgets = [ListOfMails, StatusBar, ScrollingMailView]
    }

composeView :: View
composeView =
    View
    { _vFocus = focusRing
          [ComposeFrom, ComposeTo, ComposeSubject, ListOfAttachments]
    , _vWidgets = [ ComposeFrom
                  , ComposeTo
                  , ComposeSubject
                  , StatusBar
                  , ListOfAttachments]
    }

helpView :: View
helpView =
    View
    { _vFocus = focusRing [ScrollingHelpView]
    , _vWidgets = [ScrollingHelpView]
    }

filebrowserView :: View
filebrowserView =
    View
    { _vFocus = focusRing [ListOfFiles, ManageFileBrowserSearchPath]
    , _vWidgets = [ListOfFiles, StatusBar, ManageFileBrowserSearchPath]
    }
