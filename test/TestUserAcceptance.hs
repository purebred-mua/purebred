-- This file is part of purebred
-- Copyright (C) 2017-2019 Róman Joost
--
-- purebred is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# OPTIONS_GHC -fno-warn-unused-do-bind -fno-warn-missing-signatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Data.Char (chr)
import System.IO.Temp
  ( createTempDirectory, getCanonicalTemporaryDirectory
  , emptySystemTempFile)
import Data.Either (isRight)
import Data.Functor (($>))
import Data.Foldable (for_)
import Control.Concurrent (threadDelay)
import System.IO (hPutStr, stderr)
import System.Environment (lookupEnv, getEnvironment)
import qualified System.Environment as Env
import System.FilePath.Posix
  ( (</>)
  , getSearchPath, isAbsolute, searchPathSeparator
  )
import Control.Monad (filterM, void, when)
import Data.Maybe (fromMaybe, isJust)
import Data.List (intercalate, isInfixOf, sort, sortBy)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadIO, MonadReader, runReaderT)
import Control.Monad.State (MonadState)
import System.Exit (die)

import Control.Lens (Lens', _init, _last, at, lens, preview, set, to, view)
import System.Directory
  ( copyFile, getCurrentDirectory, listDirectory, removeDirectoryRecursive
  , removeFile, doesPathExist, findExecutable
  )
import System.Posix.Files (getFileStatus, isRegularFile)
import System.Process.Typed
  (byteStringInput, proc, readProcess_, runProcess_, setEnv, setStdin)
import Test.Tasty (defaultMain)
import Test.Tasty.HUnit (assertBool, assertEqual)
import Test.Tasty.Tmux

import Data.MIME
  (MIMEMessage, createTextPlainMessage, message, mime, parse,
  headers, buildMessage)

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

type PurebredTestCase = TestCase GlobalEnv

main :: IO ()
main = do
  for_ ["purebred", "elinks"] $ \prog ->
    findExecutable prog >>= maybe (die $ "missing program: " <> prog) (\_ -> pure ())

  Env.setEnv tastyNumThreadsEnv . fromMaybe "20" =<< lookupEnv tastyNumThreadsEnv
  defaultMain $ testTmux pre post tests
  where
    tastyNumThreadsEnv = "TASTY_NUM_THREADS"

    pre = do
      dir <- mkTempDir
      setUpPurebredConfig dir
      precompileConfig dir  -- all tests can use same precompiled binary
      pure (GlobalEnv dir)

    post (GlobalEnv dir) =
      removeDirectoryRecursive dir

    tests =
      [ testUserViewsMailSuccessfully
      , testUserCanManipulateNMQuery
      , testUserCanSwitchBackToIndex
      , testUserCanAbortMailComposition
      , testSendMail
      , testSendFailureHandling
      , testCanToggleHeaders
      , testSetsMailToRead
      , testShowsAndClearsError
      , testHelp
      , testManageTagsOnMails
      , testManageTagsOnThreads
      , testConfig
      , testUpdatesReadState
      , testCanJumpToFirstListItem
      , testAddAttachments
      , testFileBrowserInvalidPath
      , testFromAddressIsProperlyReset
      , testRepliesToMailSuccessfully
      , testUserCanMoveBetweenThreads
      , testShowsMailEntities
      , testOpenCommandDoesNotKillPurebred
      , testOpenEntitiesSuccessfully
      , testPipeEntitiesSuccessfully
      , testEditingMailHeaders
      , testShowsInvalidCompositionInput
      , testShowsInvalidTaggingInput
      , testKeepDraftMail
      , testDiscardsMail
      , testShowsNewMail
      , testConfirmDialogResets
      , testCursorPositionedEndOnReply
      , testSubstringSearchInMailBody
      , testSubstringMatchesAreCleared
      , testAutoview
      , testSavesEntitySuccessfully
      , testForwardsMailSuccessfully
      , testBulkActionsOnThreadsByKeybinding
      , testBulkActionsOnThreadsByInput
      , testBulkActionsOnMailsByInput
      , testAbortedEditsResetState
      , testReloadsThreadListAfterReply
      , testAbortsCompositionIfEditorExits
      , testSearchRelated
      , testReplyRendersNonASCIIHeadersCorrectly
      , testGroupReply
      ]

testSearchRelated :: PurebredTestCase
testSearchRelated = purebredTmuxSession "searches related" $
  \step -> do
    startApplication

    capture >>= put
    assertSubstringS "Item 1 of 4"
    assertRegexS (buildAnsiRegex ["37"] ["43"] [] <> "\\sAug'17 frase@host.exa")

    step "search related"
    sendKeys "+" (Substring "Item 1 of 2") >>= put
    assertRegexS ("Query:\\s" <> buildAnsiRegex ["34"] [] [] <> "frase@host.example")

-- https://github.com/purebred-mua/purebred/issues/336
testAbortsCompositionIfEditorExits :: PurebredTestCase
testAbortsCompositionIfEditorExits = purebredTmuxSession "aborts composition if editor exits abnormally" $
  \step -> do
    setEnvVarInSession "EDITOR" "doesnotexistFoo"
    startApplication
    
    step "start composition"
    sendKeys "m" (Substring "From")

    step "accept default"
    sendKeys "Enter" (Substring "To")

    step "enter to: email"
    sendLine "user@to.test" (Substring "Subject")

    step "enter subject"
    sendLine "Draft mail subject" (Substring "Editor exited abnormally")

    -- check reply
    step "Navigate second mail"
    sendKeys "Down" (Substring "Item 2 of 4")

    step "View mail"
    sendKeys "Enter" (Substring "HOLY PUREBRED")

    step "reply to mail"
    sendKeys "r" (Substring "Editor exited abnormally")

    -- mail composition from mail view
    step "start composition from mail view"
    sendKeys "f" (Substring "To:")

    step "accept default"
    sendKeys "Enter" (Substring "Editor exited abnormally")

    step "back to thread list"
    sendKeys "Escape" (Substring "Item 2 of 4")
    
    

-- https://github.com/purebred-mua/purebred/issues/395
testReloadsThreadListAfterReply :: PurebredTestCase
testReloadsThreadListAfterReply = purebredTmuxSession "reloads list of threads" $
  \step -> do
    startApplication

    step "focus query editor"
    sendKeys ":" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "tag:inbox"))
    sendKeys "C-u" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "\\s+"))

    step "query for receiver"
    sendLine "to:frase" (Substring "Item 1 of 2")

    step "view mail"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "compose as new"
    sendKeys "e" (Substring "Testmail with whitespace in the subject")

    step "navigate to latest attachment"
    sendKeys "Down" (Substring "Item 2 of 2") >>= put
    assertSubstringS "text/html"

    step "Remove HTML part"
    sendKeys "D" (Not (Substring "text/html")) >>= put
    assertSubstringS "Item 1 of 1"

    step "send mail"
    sendKeys "y" (Substring "Item 1 of 2")

    step "check current first mail"
    sendKeys "Enter" (Substring "This is a test mail for purebred") >>= put

    assertSubstringS "Date: Thu, 17 Aug"

    step "update list of threads"
    sendKeys "Escape" (Substring "Item 1 of 2")
    sendKeys ":" Unconditional
    sendKeys "Enter" (Substring "Item 1 of 3")

    step "open first mail"
    sendKeys "Enter" (Substring "This is a test mail for purebred")


testAbortedEditsResetState :: PurebredTestCase
testAbortedEditsResetState = purebredTmuxSession "aborted edits reset editor back to initial state" $
  \step -> do
    startApplication

    step "edit search query"
    assertEditorResetsToInitialValue
      step
      ":"
      ("Query: " <> buildAnsiRegex [] ["37"] [] <> "tag:inbox")
      ("Query: " <> buildAnsiRegex [] ["34"] [] <> "tag:inbox")

    composeNewMail step

    step "edit Subject: field"
    assertEditorResetsToInitialValue
      step
      "s"
      ("Subject: " <> buildAnsiRegex [] ["37"] [] <> "Draft mail subject")
      "Subject: Draft mail subject"

    step "edit BCC: field"
    assertEditorResetsToInitialValue
      step
      "b"
      ("Bcc: " <> buildAnsiRegex [] ["37"] [])
      "Bcc:"

    step "edit CC: field"
    assertEditorResetsToInitialValue
      step
      "c"
      ("Cc: " <> buildAnsiRegex [] ["37"] [])
      "Cc:"

    step "edit From: field"
    assertEditorResetsToInitialValue
      step
      "f"
      ("From: " <> buildAnsiRegex [] ["37"] [] <> "\"Joe Bloggs\" <joe@foo.test>")
      "From: \"Joe Bloggs\" <joe@foo.test>"

    step "edit To: field"
    assertEditorResetsToInitialValue
      step
      "t"
      ("To: " <> buildAnsiRegex [] ["37"] [] <> "user@to.test")
      "To: user@to.test"


testBulkActionsOnMailsByInput :: PurebredTestCase
testBulkActionsOnMailsByInput = purebredTmuxSession "perform bulk labeling on mails by editor" $
  \step -> do
    startApplication

    step "navigate to thread with two mails"
    sendKeys "Down" (Substring "Item 2 of 4")
    sendKeys "Down" (Substring "Item 3 of 4")
    sendKeys "Enter" (Substring "Lorem ipsum dolor sit amet")

    step "toggle first mail"
    sendKeys "*" (Regex $ selectedListItem <> "\\sFeb'17.*WIP Refactor")

    step "toggle second mail"
    -- toggled *and* currently selected item
    sendKeys "*" (Regex $ buildAnsiRegex [] [] ["43"] <> "\\sFeb'17.*Re: WIP Refactor")

    step "open mail tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))
      >>= put

    -- guard against a case in which mails are already tagged with our test tag
    assertConditionS (Not (Substring "testTag"))

    step "add new tag"
    sendLine "+testTag" (
      Regex (
          "testTag"
          -- first list item starting with the tag
          <> buildAnsiRegex [] ["34"] [] <> "\\s+WIP Refactor\\s+\n"
          -- next mail/list item
          <> buildAnsiRegex [] ["37"] ["43"] <> "\\s+Feb'17.*testTag"
          <> buildAnsiRegex [] ["37"] [] <> "\\s+Re: WIP Refactor"
          )
      ) >>= put

    -- Editor is not displayed any more
    assertConditionS (Not (Substring "Labels:"))
    -- Every toggled list item is now untoggled
    assertConditionS (Not (Substring "Marked"))


testBulkActionsOnThreadsByInput :: PurebredTestCase
testBulkActionsOnThreadsByInput = purebredTmuxSession "perform bulk labeling on threads by editor" $
  \step -> do
    startApplication

    step "Toggle two thread items"
    sendKeys "*" (Regex $ toggledListItem <> "\\sAug.*Testmail with whitespace in the subject")
    -- The previous line has the same colour, so start colour matching from the first line
    sendKeys "*" (Regex $ toggledListItem <> "\\sAug.*in the subject.*\\s\\sAug.*This is Purebred")

    step "open thread tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))
      >>= put

    -- guard against a case in which mails are already tagged with our test tag
    assertConditionS (Not (Substring "testTag"))

    step "add tag"
    sendLine "+testTag" (
      Regex (
          "testTag"
          <> buildAnsiRegex [] ["37"] []
          <> "\\s+Testmail with whitespace in the subject\\s*\n"
          <> buildAnsiRegex [] ["34"] [] <> "\\s+Aug'17.*testTag"
          <> buildAnsiRegex [] ["34"] [] <> "\\s+This is Purebred\\s+\n"
          )
      )
    

testBulkActionsOnThreadsByKeybinding :: PurebredTestCase
testBulkActionsOnThreadsByKeybinding =
  purebredTmuxSession "perform bulk labeling on threads by keybinding" $
  \step -> do
    startApplication

    step "Toggle thread and list cursor moves to next list item"
    sendKeys "*" (Regex $ toggledListItem <> "\\sAug'17.*whitespace in the subject\\s+\n")
      >>= assertRegex (
        -- current selection
        buildAnsiRegex [] ["30"] ["43"] <> "\\sAug'17 rjoost@url.use.*This is Purebred\\s+\n"
        -- unselected rest
        <> newListItem <> "\\sFeb'17.*WIP Refactor"
      )

    step "Toggle thread and list cursor moves to next list item"
    sendKeys "*" (Regex $ selectedListItem <> "\\sFeb'17.*WIP Refactor")

    step "Tag toggled list items using key binding"
    sendKeys "a" (Substring "New: 3  ]")
      -- untoggled
      >>= assertRegex (buildAnsiRegex [] ["37"] [] <> "\\sAug'17.*whitespace in the subject\\s+\n")

testForwardsMailSuccessfully :: PurebredTestCase
testForwardsMailSuccessfully = purebredTmuxSession "forwards mail successfully" $
  \step -> do
    startApplication

    let subject = "[frase@host.example: Testmail with whitespace in the subject]"

    step "view mail"
    sendKeys "Enter" (Substring "This is a test mail")

    step "Start forwarding composition"
    sendKeys "f" (Regex $ "To: " <> buildAnsiRegex [] ["37"] [] <> "\\s+$")

    step "enter receipient address"
    sendLine "to_user@foo.test" (Substring "~")

    step "enter mail body"
    sendKeys "iFind attached a forwarded mail" (Substring "mail")

    step "exit insert mode in vim"
    sendKeys "Escape" (Substring "mail")

    step "exit vim"
    sendKeys ": x\r" (Substring "Attachments")

    step "start editing cc"
    sendKeys "c" (Substring "Cc")

    step "add cc email"
    sendKeys "cc_user@foo.test\r" (Substring "Cc: cc_user@foo.test")

    step "start editing bcc"
    sendKeys "b" (Regex $ "Bcc: " <> buildAnsiRegex [] ["37"] [] <> "\\s+$")

    step "add bcc email"
    sendKeys "bcc_user@foo.test\r" (Substring "Bcc: bcc_user@foo.test") >>= put

    assertRegexS "From: \"Joe Bloggs\" <joe@foo.test>\\s+$"
    assertSubstringS "To: to_user@foo.test"
    assertSubstringS "Cc: cc_user@foo.test"
    assertSubstringS "Bcc: bcc_user@foo.test"
    assertSubstringS ("Subject: " <> subject)
    assertSubstringS "text/plain"
    assertSubstringS "message/rfc822"

    step "send mail"
    sendKeys "y" (Substring "Query")

    testdir <- view envConfigDir
    let fpath = testdir </> "sentMail"

    assertMailSuccessfullyParsed fpath

    contents <- liftIO $ B.readFile fpath
    let decoded = chr . fromEnum <$> B.unpack contents
    assertSubstr subject decoded
    assertSubstr "This is a test mail" decoded
    assertSubstr "Find attached a forwarded mail" decoded

testSavesEntitySuccessfully :: PurebredTestCase
testSavesEntitySuccessfully = purebredTmuxSession "saves entity to disk successfully" $
  \step -> do
    startApplication

    let mailbody = "This is a test mail for purebred"
        bogusSavePath = "/tmp/this/path/should/not/exist"

    -- Better check and abort the test if our made up path really does
    -- exist however unlikely we think it is.
    liftIO $ assertBool "expected bogus path to not exist" <$> doesPathExist bogusSavePath

    tmpfile <- liftIO $ emptySystemTempFile "purebred_saves_entity_to_disk_successfully"

    step "show current mail body"
    sendKeys "Enter" (Substring mailbody)

    step "list attachments"
    sendKeys "v" (Substring "text/plain; charset=utf-8")

    step "show save to disk editor"
    sendKeys "s" (Substring "Save to file")

    step "enter (wrong) path"
    sendLine bogusSavePath (Substring "openBinaryFile: does not exist")

    step "show save to disk editor (again)"
    sendKeys "s" (Regex $ "Save to file:\\s+" <> buildAnsiRegex [] ["37"] [] <> "\\s+")

    step "enter (correct) path"
    sendLine tmpfile (Substring "Attachment saved")

    snapshot
    assertConditionS (Not (Substring "Save to file"))

    contents <- liftIO $ B.readFile tmpfile
    let decoded = chr . fromEnum <$> B.unpack contents
    assertSubstr mailbody decoded


testAutoview :: PurebredTestCase
testAutoview = purebredTmuxSession "automatically copies output for display" $
  \step -> do
    startApplication

    step "search for HTML mail"
    findMail step "subject:\"HTML mail\""

    step "open HTML mail"
    sendKeys "Enter" (Substring "This is a HTML mail for purebred in which the HTML part contains")

    step "use as reply"
    sendKeys "r" (Regex ">\\s+This is a HTML mail for purebred")


testSubstringMatchesAreCleared :: PurebredTestCase
testSubstringMatchesAreCleared = purebredTmuxSession "substring match indicator only shown on mail" $
  \step -> do
    startApplication

    step "No match indicator is shown"
    snapshot
    assertRegexS "New:\\s[0-9]\\s+\\]\\s+Threads"

    step "search for Lorem mail"
    sendKeys ":" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "tag:inbox"))
    sendKeys "C-u" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "\\s+"))

    step "enter free text search"
    sendLine "Lorem ipsum" (Substring "Item 1 of 1")

    step "show mail contents"
    sendKeys "Enter" (Substring "Lorem ipsum dolor sit amet, consectetur")

    step "show substring search editor"
    sendKeys "/" (Substring "Search for")

    step "enter needle and show results"
    sendKeys "et\r" (Substring "1 of 20 matches")

    step "begin substring search"
    sendKeys "/" (Substring "Search for")

    step "enter empty search string (reset search)"
    sendKeys "\r" (Not (Substring "matches ]"))

    step "go back to threads"
    sendKeys "Escape" (Regex "New:\\s[0-9]\\s+\\]\\s+Threads")


testSubstringSearchInMailBody :: PurebredTestCase
testSubstringSearchInMailBody = purebredTmuxSession "search for substrings in mailbody" $
  \step -> do
    startApplication

    step "search for Lorem mail"
    sendKeys ":" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "tag:inbox"))
    sendKeys "C-u" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "\\s+"))

    step "enter free text search"
    sendLine "Lorem ipsum" (Substring "Item 1 of 1")

    step "show mail contents"
    sendKeys "Enter" (Substring "Lorem ipsum dolor sit amet, consectetur")

    step "show substring search editor"
    sendKeys "/" (Substring "Search for")

    step "enter needle and show results"
    sendKeys "et\r" (Regex ("am"
                            <> buildAnsiRegex [] ["32"] ["47"] <> "et"
                            <> buildAnsiRegex [] ["39"] ["49"] <> ", consect"
                            <> buildAnsiRegex [] ["37"] ["42"] <> "et"
                            <> buildAnsiRegex [] ["39"] ["49"] <> "ur"))

    step "highlight next search result"
    sendKeys "n" (Regex ("am"
                         <> buildAnsiRegex [] ["37"] ["42"] <> "et"
                         <> buildAnsiRegex [] ["39"] ["49"] <> ", consect"
                         <> buildAnsiRegex [] ["32"] ["47"] <> "et"
                         <> buildAnsiRegex [] ["39"] ["49"] <> "ur"))

    step "focus search input editor again"
    sendKeys "/" (Regex (buildAnsiRegex [] ["33"] [] <> "Search for:\\s"
                         <> buildAnsiRegex [] ["37"] [] <> "\\s+$"))

    step "search for different needle"
    sendKeys "Lorem\r" (Regex ("\""
                            <> buildAnsiRegex [] ["32"] ["47"] <> "Lorem"
                            <> buildAnsiRegex [] ["39"] ["49"] <> " ipsum"))

    step "clear all highlights"
    sendKeys "Enter" (Substring "Lorem ipsum dolor sit amet, consectetur")


testCursorPositionedEndOnReply :: PurebredTestCase
testCursorPositionedEndOnReply = purebredTmuxSession "cursor positioned on EOL when replying" $
  \step -> do
    startApplication

    step "pick first mail"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "start replying"
    sendKeys "r" (Substring "> This is a test mail for purebred")

    step "exit vim"
    sendKeys ": x\r" (Substring "Attachments")

    step "focus from field"
    sendKeys "f" (Regex $ "From: " <> buildAnsiRegex [] ["37"] [] <> "\"Joe Bloggs\" <joe@foo.test>")
    sendKeys ", fromuser@foo.test\r" (Substring $ "From: "
                                      <> "\"Joe Bloggs\" <joe@foo.test>, fromuser@foo.test")

    step "user can change to header"
    sendKeys "t" (Regex $ "To: " <> buildAnsiRegex [] ["37"] [] <> "frase@host.example")

    step "append an additional from email"
    sendKeys ", touser@foo.test\r" (Substring "To: frase@host.example, touser@foo.test")

    step "change subject"
    sendKeys "s" (Regex $ "Subject: " <> buildAnsiRegex [] ["37"] [] <> ".*subject\\s+$")

    step "enter subject"
    sendKeys " appended\r" (Substring "Subject: Re: Testmail with whitespace in the subject appended")


testConfirmDialogResets :: PurebredTestCase
testConfirmDialogResets = purebredTmuxSession "confirm dialog resets state" $
  \step -> do
    startApplication

    composeNewMail step

    step "abort composition"
    sendKeys "q" (Substring "Keep draft?")

    step "choose Discard"
    sendKeys "Tab" (Substring "Discard")

    step "confirm discard"
    sendKeys "Enter" (Substring "Testmail")

    composeNewMail step

    step "abort composition"
    sendKeys "q" (Regex (buildAnsiRegex [] ["30"] ["42"] <> "\\s+Keep" ))


-- Note: The most time in this test is spend on waiting. The default
-- time for the indicator to refresh is 5 seconds.
testShowsNewMail :: PurebredTestCase
testShowsNewMail = purebredTmuxSession "shows newly delivered mail" $
  \step -> do
    startApplication

    step "shows new mails"
    sendKeys "Down" (Substring "New: 4")

    notmuchcfg <- view envNotmuchConfig

    let
        m = set (headers . at "subject") (Just "new mail notification") $ createTextPlainMessage "Hello there"
        rendered = toLazyByteString (buildMessage m)
        config = setStdin (byteStringInput rendered) $ proc "notmuch"
            [ "--config=" <> notmuchcfg
            , "insert"
            , "--folder"
            , "tmp"
            , "--create-folder"
            ]
    void $ readProcess_ config

    step "shows new delivered mail"
    sendKeys "Up" (Substring "New: 5")

    -- reload mails to see the new e-mail
    step "focus query widget"
    sendKeys ":" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "tag:inbox"))

    step "view mail"
    sendKeys "Enter" (Substring "new mail notification")


testShowsInvalidTaggingInput :: PurebredTestCase
testShowsInvalidTaggingInput = purebredTmuxSession "shows errors when tagging" $
  \step -> do
    startApplication

    step "start tagging"
    sendKeys "`" (Regex ("Labels: " <> buildAnsiRegex [] ["37"] []))

    step "enter invalid tag input"
    sendKeys "=," (Substring "Failed reading: unexpected ',' at offset 1")

    step "clear"
    sendKeys "BSpace" (Regex ("Labels: " <> buildAnsiRegex [] ["37"] [] <> "="))

    step "exit editor"
    sendKeys "C-g" (Substring "Query")

    step "open thread"
    sendKeys "Enter" (Substring "Testmail with whitespace")

    step "start tagging"
    sendKeys "`" (Regex ("Labels: " <> buildAnsiRegex [] ["37"] []))

    step "enter invalid tag input"
    sendKeys "=," (Substring "Failed reading: unexpected ',' at offset 1")

    step "clear"
    sendKeys "BSpace" (Regex ("Labels: " <> buildAnsiRegex [] ["37"] [] <> "="))

testShowsInvalidCompositionInput :: PurebredTestCase
testShowsInvalidCompositionInput = purebredTmuxSession "shows errors when composing" $
  \step -> do
    startApplication
    step "start composition"
    sendKeys "m" (Substring "From")

    step "trigger error"
    sendKeys "<" (Substring "Failed reading")

    step "continue"
    sendKeys "BSpace" (Substring "Purebred: (0,27)")
    sendKeys "Enter" (Substring "To:")

    step "trigger error"
    sendKeys "," (Substring "Failed reading")

    step "continue"
    sendKeys "BSpace" (Substring "Purebred: (0,0)")
    sendKeys "Enter" (Substring "Subject:")

    step "leave empty subject"
    sendKeys "Enter" (Substring "~")

    step "enter mail body"
    sendKeys "iThis is a test body" (Substring "body")

    step "exit insert mode in vim"
    sendKeys "Escape" (Substring "body")

    step "exit vim"
    sendKeys ": x\r" (Substring "text/plain")

    step "focus from field"
    sendKeys "f" (Regex $ "From:\\s"
                  <> buildAnsiRegex [] ["37"] []
                  <> "\"Joe Bloggs\" <joe@foo.test>")

    step "trigger error"
    sendKeys "," (Substring "Failed reading: unexpected ',' at offset 27")

    step "abort editing"
    sendKeys "C-g" (Substring "ComposeView-Attachments")

    step "focus to field"
    sendKeys "t" (Regex $ "To:\\s" <> buildAnsiRegex [] ["37"] [])

    step "trigger error"
    sendKeys "," (Substring "Failed reading")

    step "abort editing"
    sendKeys "C-g" (Substring "ComposeView-Attachments")
  

testDiscardsMail :: PurebredTestCase
testDiscardsMail = purebredTmuxSession "discards draft mail" $
  \step -> do
    startApplication

    composeNewMail step

    step "abort composition"
    sendKeys "Escape" (Substring "Keep draft?")

    step "choose Discard"
    sendKeys "Tab" (Substring "Discard")

    step "confirm discard"
    sendKeys "Enter" (Substring "Testmail")

    step "no draft mail exists in Maildir"
    maildir <- view envMaildir
    assertFileAmountInMaildir (maildir </> "Drafts" </> "new") 0

testKeepDraftMail :: PurebredTestCase
testKeepDraftMail = purebredTmuxSession "compose mail from draft" $
  \step -> do
    startApplication

    composeNewMail step

    step "abort composition"
    sendKeys "q" (Substring "Keep draft?")

    step "confirm Keep"
    sendKeys "Enter" (Substring "Draft saved")

    step "assert draft exists"
    maildir <- view envMaildir
    assertFileAmountInMaildir (maildir </> "Drafts" </> "new") 1

    step "search for draft"
    sendKeys ":" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "tag:inbox"))
    sendKeys "C-u" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "\\s+"))

    step "enter new tag"
    sendLine "tag:draft" (Substring "Item 1 of 1")

    step "view mail"
    sendKeys "Enter" (Substring "Draft mail subject")

    step "edit as new"
    sendKeys "e" (Regex "From: \"Joe Bloggs\" <joe@foo.test>") >>= put

    assertSubstringS "To: user@to.test"
    assertSubstringS "Subject: Draft mail subject"

    step "assert draft has been removed"
    mdir <- view envMaildir
    assertFileAmountInMaildir (mdir </> "Drafts" </> "new") 0

    step "send mail"
    sendKeys "y" (Substring "Query")

    testdir <- view envConfigDir
    let fpath = testdir </> "sentMail"
    contents <- liftIO $ B.readFile fpath
    let decoded = chr . fromEnum <$> B.unpack contents
    assertSubstr "This is a test body" decoded

testEditingMailHeaders :: PurebredTestCase
testEditingMailHeaders = purebredTmuxSession "user can edit mail headers" $
  \step -> do
    startApplication

    step "start composition"
    sendKeys "m" (Substring "From")

    step "accept default"
    sendKeys "Enter" (Substring "To")

    step "enter to: email"
    sendLine "user@to.test" (Substring "Subject")

    step "leave default"
    sendKeys "Enter" (Substring "~")

    step "enter mail body"
    sendKeys "iThis is a test body" (Substring "body")

    step "exit insert mode in vim"
    sendKeys "Escape" (Substring "body")

    step "exit vim"
    sendLine ": x" (Substring "text/plain")
      >>= assertSubstring "From: \"Joe Bloggs\" <joe@foo.test>"

    step "user can change from header"
    sendKeys "f" (Regex $ "From: " <> buildAnsiRegex [] ["37"] [] <> "\"Joe Bloggs\" <joe@foo.test>")

    let lastLineIsStatusLine = Regex "Purebred:.*ComposeView-Attachments\\s+$^$"
    step "append an email"
    sendLine ", testuser@foo.test" lastLineIsStatusLine
      >>= assertSubstring "From: \"Joe Bloggs\" <joe@foo.test>, testuser@foo.test"

    step "user can change to header"
    sendKeys "t" (Regex $ "To: " <> buildAnsiRegex [] ["37"] [] <> "user@to.test")

    step "append an additional from email"
    sendLine ", testuser@foo.test" lastLineIsStatusLine
      >>= assertSubstring "To: user@to.test, testuser@foo.test"

    step "user can add cc header"
    sendKeys "c" (Substring "Cc")

    step "enter cc: email"
    sendLine "user@cc.test" lastLineIsStatusLine
      >>= assertSubstring "Cc: user@cc.test"

    step "user can change cc header"
    sendKeys "c" (Regex $ "Cc: " <> buildAnsiRegex [] ["37"] [] <> "user@cc.test")

    step "append an additional from email"
    sendLine ", testuser@foo.test" lastLineIsStatusLine
      >>= assertSubstring "Cc: user@cc.test, testuser@foo.test"

    step "user can add bcc header"
    sendKeys "b" (Substring "Bcc")

    step "enter bcc: email"
    sendLine "user@bcc.test" lastLineIsStatusLine
      >>= assertSubstring "Bcc: user@bcc.test"

    step "user can change bcc header"
    sendKeys "b" (Regex $ "Bcc: " <> buildAnsiRegex [] ["37"] [] <> "user@bcc.test")

    step "append an additional from email"
    sendLine ", testuser@foo.test" lastLineIsStatusLine
      >>= assertSubstring "Bcc: user@bcc.test, testuser@foo.test"

    step "change subject"
    sendKeys "s" (Regex $ "Subject: " <> buildAnsiRegex [] ["37"] [] <> "")

    step "enter subject"
    sendLine "foo subject" lastLineIsStatusLine
      >>= assertSubstring "Subject: foo subject"

testPipeEntitiesSuccessfully :: PurebredTestCase
testPipeEntitiesSuccessfully = purebredTmuxSession "pipe entities successfully" $
  \step -> do
    setEnvVarInSession "LESS" ""
    startApplication

    step "open thread"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "show entities"
    sendKeys "v" (Substring "text/plain")

    step "pipe to"
    sendKeys "|" (Substring "Pipe to")

    step "use less"
    sendLine "less" (Regex ("This is a test mail for purebred"
                             <> buildAnsiRegex [] ["37"] []
                             <> "\\s+"
                             <> buildAnsiRegex ["7"] ["39"] []
                             <> "\\(END\\)"))

testOpenEntitiesSuccessfully :: PurebredTestCase
testOpenEntitiesSuccessfully = purebredTmuxSession "open entities successfully" $
  \step -> do
    setEnvVarInSession "LESS" ""
    startApplication

    step "open thread"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "show entities"
    sendKeys "v" (Substring "text/plain")

    step "open one entity"
    sendKeys "o" (Substring "Open With")
    sendLine "less" (Regex ("This is a test mail for purebred"
                            <> buildAnsiRegex [] ["37"] []
                            <> "\\s+"
                            <> buildAnsiRegex ["7"] ["39"] []
                            <> ".*purebred.*END"))

testOpenCommandDoesNotKillPurebred :: PurebredTestCase
testOpenCommandDoesNotKillPurebred = purebredTmuxSession "open attachment does not kill purebred" $
  \step -> do
    startApplication

    step "open thread"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "show entities"
    sendKeys "v" (Substring "text/plain")

    step "open with"
    sendKeys "o" (Substring "Open With")

    step "Open with bogus command"
    sendLine "asdfasdfasdf" (Substring "ProcessError")

testShowsMailEntities :: PurebredTestCase
testShowsMailEntities = purebredTmuxSession "shows mail entities successfully" $
  \step -> do
    startApplication

    step "open thread"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "show entities"
    sendKeys "v" (Substring "text/plain")

    step "select the second entity"
    sendKeys "j" (Substring "text/html")

    step "close the list of entities"
    out <- sendKeys "q" (Substring "This is a test mail for purebred")

    -- poor mans (?!text)
    assertRegex "[^t][^e][^x][^t]" out

testUserCanMoveBetweenThreads :: PurebredTestCase
testUserCanMoveBetweenThreads = purebredTmuxSession "user can navigate between threads" $
  \step -> do
    startApplication
    -- assert that the first mail is really the one we're later navigating back
    -- to
    snapshot
    assertRegexS (buildAnsiRegex [] ["37"] ["43"] <> "\\sAug'17.*Testmail with whitespace")

    step "View Mail"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "Navigate down the threads list"
    sendKeys "J" (Substring "HOLY PUREBRED")

    step "Navigate up the threads list"
    sendKeys "K" (Substring "This is a test mail for purebred")

testRepliesToMailSuccessfully :: PurebredTestCase
testRepliesToMailSuccessfully = purebredTmuxSession "replies to mail successfully" $
  \step -> do
    let subject = "Testmail with whitespace in the subject"
    testdir <- view envConfigDir
    startApplication

    step "pick first mail"
    sendKeys "Enter" (Substring "This is a test mail for purebred") >>= put

    assertSubstringS "From: <frase@host.example>"
    assertSubstringS "To: <roman@host.example>"
    assertSubstringS ("Subject: " <> subject)

    step "start replying"
    sendKeys "r" (Substring "> This is a test mail for purebred")

    step "exit vim"
    sendLine ": x" (Substring "Attachments") >>= put

    assertRegexS "From: \"Joe Bloggs\" <joe@foo.test>\\s+$"
    assertSubstringS "To: frase@host.example"
    assertRegexS "Cc:\\s*$"  -- Cc should be empty
    assertSubstringS ("Subject: Re: " <> subject)

    -- https://github.com/purebred-mua/purebred/issues/379
    step "edit the mail one more time"
    sendKeys "e" (Substring "> This is a test mail for purebred")

    step "insert body"
    sendKeys "oThis is more information" (Substring "This is more information")
    sendKeys "Escape" Unconditional

    step "exit vim"
    sendLine ": x" (Substring "Item 1 of 1")

    step "send mail"
    sendKeys "y" (Substring "Query")

    let fpath = testdir </> "sentMail"
    contents <- liftIO $ B.readFile fpath
    let decoded = chr . fromEnum <$> B.unpack contents
    assertSubstr ("Subject: Re: " <> subject) decoded
    assertSubstr "From: \"Joe Bloggs\" <joe@foo.test>" decoded
    assertSubstr "To: frase@host.example" decoded
    assertSubstr "> This is a test mail for purebred" decoded

testFromAddressIsProperlyReset :: PurebredTestCase
testFromAddressIsProperlyReset = purebredTmuxSession "from address is reset to configured identity" $
  \step -> do
    startApplication

    step "Start composing"
    sendKeys "m" (Substring "Joe Bloggs")

    step "abort editing"
    sendKeys "Escape" (Substring "tag:inbox")

    step "Start composing again"
    sendKeys "m" (Substring "Joe Bloggs")

testCanJumpToFirstListItem :: PurebredTestCase
testCanJumpToFirstListItem = purebredTmuxSession "can jump to first and last mail" $
  \step -> do
    startApplication

    step "Jump to last mail"
    sendKeys "G" (Substring "4 of 4")

    step "Jump to first mail"
    sendKeys "1" (Substring "1 of 4")

testUpdatesReadState :: PurebredTestCase
testUpdatesReadState = purebredTmuxSession "updates read state for mail and thread" $
  \step -> do
    startApplication

    findMail step "subject:WIP Refactor"

    step "view unread mail in thread"
    sendKeys "Enter" (Substring "WIP Refactor")

    step "view next unread in thread"
    sendKeys "Down" (Substring "2 of 2")

    step "go back to thread list which is now read"
    sendKeys "q" (Regex (buildAnsiRegex [] ["30"] ["43"] <> T.encodeUtf8 " Feb'17\\sRóman\\sJoost\\s+\\(2\\)"))

    step "set one mail to unread"
    sendKeys "Enter" (Substring "Beginning of large text")
    sendKeys "t" (Regex (buildAnsiRegex [] ["37"] []
                           <> "\\sRe: WIP Refactor\\s+"
                           <> buildAnsiRegex [] ["34"] ["49"]))

    step "returning to thread list shows entire thread as unread"
    sendKeys "q" (Regex (buildAnsiRegex [] ["37"] [] <> "\\sWIP Refactor\\s"))

testConfig :: PurebredTestCase
testConfig = purebredTmuxSession "test custom config" $
  \step -> do
    -- Set a short command prompt, to a value otherwise unlikely to
    -- appear, so that we can easily check for program termination.
    let unlikelyString = "unlikely"
    sendKeys ("PS1=" <> unlikelyString <> "$ \r") (Substring unlikelyString)
    startApplication

    step "archive thread"
    sendKeys "a" (Substring "archive")

    step "quit"
    sendKeys "q" Unconditional

    -- Wait a bit so that purebred, which may not yet have
    -- terminated, does not eat the upcoming keystroke(s)
    liftIO $ threadDelay 200000  -- 0.2 seconds

    -- Press Enter again to deal with case where cursor is not at
    -- column 0, which could cause target string to be split.
    sendKeys "Enter" (Substring unlikelyString)

-- https://github.com/purebred-mua/purebred/issues/391
testFileBrowserInvalidPath :: PurebredTestCase
testFileBrowserInvalidPath = purebredTmuxSession "file browser handles invalid path input" $
  \step -> do
    startApplication
    composeNewMail step

    step "start file browser"
    cwd <- B.pack <$> liftIO getCurrentDirectory
    sendKeys "a" (Regex $ "Path:\\s" <> buildAnsiRegex [] ["34"] [] <> cwd)

    step "focus search path editor"
    sendKeys ":" (Regex $ "Path:\\s" <> buildAnsiRegex [] ["37"] [] <> cwd)

    step "clear input and enter invalid directory"
    sendKeys "C-u" Unconditional
    sendLine "asdfasdf" (Substring "asdfasdf does not exist")

testAddAttachments :: PurebredTestCase
testAddAttachments = purebredTmuxSession "use file browser to add attachments" $
  \step -> do
    testdir <- view envConfigDir

    -- To be resilient against differences in list contents between
    -- git and sdist, list the directory ourselves to work out what
    -- the final entry should be.  Note that dirs come first and the
    -- files are sorted case insensitively in the filebrowser widget.
    let caseInsensitive a b = compare (T.toLower a)  (T.toLower b)
    files <- sortBy caseInsensitive . fmap T.pack <$> liftIO (
      getSourceDirectory >>= listDirectory
      >>= filterM (fmap isRegularFile . getFileStatus) )
    let
      lastFile = fromMaybe "MISSING" $ preview (_last . to T.encodeUtf8) files
      secondLastFile = fromMaybe "MISSING" $ preview (_init . _last . to T.encodeUtf8) files

    startApplication
    composeNewMail step

    step "start file browser"
    cwd <- B.pack <$> liftIO getCurrentDirectory
    sendKeys "a" (Regex $ "Path: " <> buildAnsiRegex [] ["34"] [] <> cwd)

    step "jump to the end of the list"
    sendKeys "G" (Regex $ buildAnsiRegex [] [] ["43"] <> lastFile)

    step "add first selected file"
    sendKeys "Enter" (Substring lastFile)

    step "up to select mail body"
    sendKeys "Up" (Substring "Item 1 of 2")

    -- edit the mail body a few times to check if the code not mistakenly adds
    -- the same mail body as an attachment
    step "edit mail body text"
    sendKeys "e" (Substring "test body")

    step "append to mail body"
    sendKeys "i. foo" (Substring "foo")

    step "exit insert mode in vim"
    sendKeys "Escape" (Substring "foo")

    step "exit vim"
    sendKeys ": x\r" (Substring "Attachments")

    step "edit mail body text"
    sendKeys "e" (Substring "test body")

    step "append to mail body"
    sendKeys "i. foo" (Substring "foo")

    step "exit insert mode in vim"
    sendKeys "Escape" (Substring "foo")

    step "exit vim"
    sendKeys ": x\r" (Substring "Item 1 of 2")

    -- try removing attachments
    step "select the attachment"
    sendKeys "Down" (Substring "Item 2 of 2") >>= put
    assertRegexS (buildAnsiRegex [] ["43"] [] <> "\\sA\\s" <> lastFile)

    step "remove the attachment"
    sendKeys "D" (Not (Substring lastFile)) >>= put
    assertSubstringS "Item 1 of 1"

    step "try to remove the last attachment"
    sendKeys "D" (Substring "You may not remove the only attachment")

    -- add the attachment again and send it
    step "start file browser"
    sendKeys "a" (Regex $ "Path: " <> buildAnsiRegex [] ["34"] [] <> cwd)

    step "jump to the end of the list"
    sendKeys "G" (Regex $ buildAnsiRegex [] [] ["43"] <> lastFile)

    step "select the file"
    sendKeys "Space" (Regex $ buildAnsiRegex [] [] ["43"] <> lastFile <> "*")

    step "move one item up"
    sendKeys "Up" (Regex $ buildAnsiRegex [] [] ["43"] <> secondLastFile)

    step "add selected files"
    out <- sendKeys "Enter" (Substring "Item 3 of 3")
    assertSubstring secondLastFile out

    step "send mail"
    sendKeys "y" (Substring "Query")

    let fpath = testdir </> "sentMail"
    contents <- liftIO $ B.readFile fpath
    let decoded = chr . fromEnum <$> B.unpack contents
    assertSubstr "attachment; filename" decoded
    assertSubstr (B.unpack secondLastFile) decoded
    assertSubstr (B.unpack lastFile) decoded
    assertSubstr "This is a test body" decoded

testManageTagsOnMails :: PurebredTestCase
testManageTagsOnMails = purebredTmuxSession "manage tags on mails" $
  \step -> do
    startApplication

    step "view mail in thread"
    sendKeys "Enter" (Substring "Testmail")

    step "focus command to show mail tags"
    sendKeys "`" (Regex (buildAnsiRegex [] ["37"] []))

    step "enter new tag"
    sendLine "+inbox +foo +bar" (Regex ("foo"
                             <> buildAnsiRegex [] ["30"] []
                             <> "\\s"
                             <> buildAnsiRegex [] ["36"] []
                             <> "bar"))
      >>= assertSubstring "This is a test mail"

    step "go back to list of threads"
    sendKeys "Escape" (Substring "List of Threads")

    -- find newly tagged mail
    step "focus tag search"
    sendKeys ":" (Regex (buildAnsiRegex [] ["37"] [] <> "tag"))
    sendKeys "C-u" (Regex (buildAnsiRegex [] ["37"] []))

    step "enter tag to search `foo and bar`"
    sendLine "tag:foo and tag:bar" (Substring "tag:foo and tag:bar")

    step "view mail in thread"
    sendKeys "Enter" (Substring "Testmail")

    step "attempt to add a new tag"
    sendKeys "`" (Regex (buildAnsiRegex [] ["37"] []))

    step "cancel tagging and expect old UI"
    -- instead of asserting the absence of the tagging editor, we assert the
    -- last visible "item" in the UI followed by whitespace.
    sendKeys "Escape" (Regex "This is a test mail for purebred\\s+$")

testManageTagsOnThreads :: PurebredTestCase
testManageTagsOnThreads = purebredTmuxSession "manage tags on threads" $
  \step -> do
    startApplication

    -- setup: tag the mails in the thread with two different tags and then
    -- tag the thread as a whole with a new tag. All mails should keep their
    -- distinct tags, while having received a new tag.
    step "navigate to thread"
    sendKeys "Down" (Substring "Item 2 of 4")
    sendKeys "Down" (Substring "Item 3 of 4")

    step "show thread mails"
    sendKeys "Enter" (Substring "ViewMail")

    step "open mail tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    step "add new tag"
    sendLine "+archive" (Substring "archive")

    step "move to second mail"
    sendKeys "Down" (Substring "Item 2 of 2")

    step "open mail tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    step "add new tag"
    sendLine "+replied -inbox" (Substring "replied")

    step "thread tags shows new tags"
    sendKeys "Escape" (Regex ("archive"
                              <> buildAnsiRegex [] ["30"] []
                              <> "\\s"
                              <> buildAnsiRegex [] ["36"] []
                              <> "replied"))

    step "open thread tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    step "remove tag"
    -- "cheating" here a bit, since just invoking tmux with sending literally
    -- "-only" will fail due to tmux parsing it as an argument, but the mail is
    -- already tagged with "thread" so the additional adding won't do anything
    sendLine "+thread" (Regex ("archive"
                             <> buildAnsiRegex [] ["30"] []
                             <> "\\s"
                             <> buildAnsiRegex [] ["36"] [] <> "replied" <> buildAnsiRegex [] ["30"] []
                             <> "\\s"
                             <> buildAnsiRegex [] ["36"] [] <> "thread"))

    step "show thread mails"
    sendKeys "Enter" (Substring "ViewMail")

    step "second mail shows old tag"
    sendKeys "Escape" (Regex ("replied"
                              <> buildAnsiRegex [] ["30"] []
                              <> "\\s"
                              <> buildAnsiRegex [] ["36"] []
                              <> "thread"
                              <> buildAnsiRegex [] ["30"] []
                              <> "\\sWIP Refactor"))

    step "open thread tag editor"
    sendKeys "`" (Regex ("Labels:." <> buildAnsiRegex [] ["37"] []))

    step "abort editing"
    sendKeys "Escape" (Substring "Query")

testHelp :: PurebredTestCase
testHelp = purebredTmuxSession "help view" $
  \step -> do
    startApplication

    step "shows Keybindings"
    sendKeys "?" (Regex "Escape>\\s+cancel")

    sendKeys "Escape" (Substring "Purebred")

testShowsAndClearsError :: PurebredTestCase
testShowsAndClearsError = purebredTmuxSession "shows and clears error" $
  \step -> do
    startApplication

    testmdir <- view envMaildir
    liftIO $ removeFile (testmdir <> "/new/1502941827.R15455991756849358775.url")

    step "open thread"
    sendKeys "Enter" (Substring "Testmail")

    step "shows error message"
    sendKeys "Enter" (Substring "FileReadError")
      >>= assertRegex "open(Binary)?File:.*does not exist"

    step "error is cleared with next registered keybinding"
    sendKeys "Up" (Substring "Purebred: Item 1 of 4")

testSetsMailToRead :: PurebredTestCase
testSetsMailToRead = purebredTmuxSession "user can toggle read tag" $
  \step -> do
    startApplication

    step "open thread"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "first unread mail is opened"
    sendKeys "Escape" (Substring "List of Threads")
      >>= assertRegex (buildAnsiRegex [] ["30"] [] <> ".*Testmail")

    step "show mail"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "toggle single mail back to unread (bold again)"
    sendKeys "t" (Regex (buildAnsiRegex [] ["37"] [] <> ".*Testmail"))

testCanToggleHeaders :: PurebredTestCase
testCanToggleHeaders = purebredTmuxSession "user can toggle Headers" $
  \step -> do
    startApplication
    step "open thread"
    sendKeys "Enter" (Substring "Testmail")

    step "view mail"
    sendKeys "Enter" (Substring "This is a test mail")

    step "toggle to show all headers"
    sendKeys "h" (Regex "[Rr]eturn-[Pp]ath")

    step "toggle filtered headers"
    out <- sendKeys "h" (Substring "This is a test mail")
    assertRegex "Purebred.*\n.*[Ff]rom" out

testUserViewsMailSuccessfully :: PurebredTestCase
testUserViewsMailSuccessfully = purebredTmuxSession "user can view mail" $
  \step -> do
    startApplication
    step "shows tag"
    snapshot
    assertSubstringS "inbox"
    assertSubstringS "Testmail with whitespace in the subject"

    step "open thread"
    sendKeys "Enter" (Substring "Testmail with whitespace in the subject")

    step "view mail"
    sendKeys "Enter" (Substring "This is a test mail")

    step "go back to thread list"
    sendKeys "q" (Substring "WIP Refactor")

    step "Move down to threaded mails"
    sendKeys "Down" (Substring "Purebred: Item 2 of 4")
    sendKeys "Down" (Substring "Purebred: Item 3 of 4")
    sendKeys "Enter" (Substring "Re: WIP Refactor")

    step "Scroll down"
    sendKeys "Enter" (Substring "Beginning of large text")
    sendKeys "Space" (Substring "Sed ut perspiciatis")

    step "go to next unread mail"
    sendKeys "j" (Substring "Re: WIP Refactor")

    step "Scroll down (again)"
    sendKeys "Space" (Substring "Sed ut perspiciatis")

    step "go to previous mail with reset scroll state"
    sendKeys "k" (Regex "Subject:\\s.*WIP Refactor")

testUserCanManipulateNMQuery :: PurebredTestCase
testUserCanManipulateNMQuery =
   purebredTmuxSession
        "manipulating notmuch search query results in empty index" $
        \step -> do
          startApplication
          step "focus command"
          sendKeys ":" (Regex (buildAnsiRegex [] ["37"] [] <> "tag"))

          step "delete all input"
          sendKeys "C-u" (Regex ("Query: " <> buildAnsiRegex [] ["37"] []))

          step "search for non existing tags yielding no results"
          sendLine "does not match anything" (Substring "No items")

          step "search for mail correctly tagged"
          sendKeys ":" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "does"))
          sendKeys "C-u" (Regex (buildAnsiRegex [] ["37"] []))

          step "enter new tag"
          sendLine "tag:replied" (Substring "Item 1 of 1")

          step "open thread"
          sendKeys "Enter" (Substring "This is Purebred")

          step "view currently selected mail"
          sendKeys "Enter" (Substring "HOLY PUREBRED")

testUserCanSwitchBackToIndex :: PurebredTestCase
testUserCanSwitchBackToIndex =
  purebredTmuxSession "user can switch back to mail index during composition" $
        \step -> do
            startApplication
            step "start composition"
            sendKeys "m" (Substring "From")

            step "enter from email"
            sendKeys "C-a" Unconditional
            sendKeys "C-k" Unconditional
            sendKeys "testuser@foo.test\r" (Substring "To")

            step "enter to: email"
            sendKeys "user@to.test\r" (Substring "Subject")

            step "enter subject"
            sendKeys "test subject\r" (Substring "~")

            step "enter mail body"
            sendKeys "iThis is a test body" (Substring "body")

            step "exit insert mode in vim"
            sendKeys "Escape" (Substring "body")

            step "exit vim"
            sendKeys ": x\r" (Regex "From: testuser@foo.test")

            step "switch back to index"
            sendKeys "Tab" (Substring "Testmail")

            step "switch back to the compose editor"
            sendKeys "Tab" (Substring "test subject")

testUserCanAbortMailComposition :: PurebredTestCase
testUserCanAbortMailComposition =
  purebredTmuxSession "user can abort composing mail" $
        \step -> do
            startApplication

            composeNewMail step

            step "abort mail"
            sendKeys "q" (Substring "Keep draft?")

            step "choose discard"
            -- TODO: buildAnsiRegex will cause the generated Regex not
            -- to match. Maybe not \\s+ even though raw it looks like
            -- there is white space?
            -- see https://github.com/purebred-mua/tasty-tmux/issues/8
            sendKeys "Tab" (Substring "Discard")

            step "confirm discard"
            sendKeys "Enter" (Substring "Testmail")

            step "start composition again"
            sendKeys "m" (Substring "From")
            sendKeys "Enter" (Regex ("To:\\s" <> buildAnsiRegex [] ["37"] []))

            step "enter to: email"
            sendKeys "new@second.test\r" (Regex ("Subject:\\s" <> buildAnsiRegex [] ["37"] []))

            step "enter subject"
            sendKeys "test subject\r" (Regex "~\\s+")

            step "enter mail body"
            sendKeys "iThis is my second mail" Unconditional

            step "exit insert mode in vim"
            sendKeys "Escape" Unconditional

            step "exit vim"
            sendKeys ": x\r" (Substring "text/plain") >>= put

            assertSubstringS "To: new@second.test"
            assertSubstringS "Subject: test subject"

            step "edit body"
            sendKeys "e" (Regex "This is my second mail\\s+")


testSendMail :: PurebredTestCase
testSendMail =
  purebredTmuxSession "sending mail successfully" $
        \step -> do
          testdir <- view envConfigDir
          mdir <- view envMaildir
          startApplication
          composeNewMail step

          step "user can re-edit body"
          sendKeys "e" (Substring "This is a test body")

          step "Writes more text"
          sendKeys "i. More text" (Substring "text")

          step "exit insert mode in vim"
          sendKeys "Escape" (Substring "body")

          step "exit vim"
          sendKeys ": x\r" (Regex ("text/plain; charset=us-ascii\\s" <> buildAnsiRegex [] ["34"] ["49"] <> "\\s+"))

          -- pre-check before we sent:
          --   * Drafts is empty before sending
          --   * Sent folder doesn't exist yet
          --
          step "Drafts is empty before sending"
          assertFileAmountInMaildir (mdir </> "Drafts" </> "new") 0

          step "Sent folder doesn't exist yet"
          files <- liftIO $ listDirectory mdir
          liftIO $
            assertEqual
            "expected no maildir directories"
            (sort ["Drafts", ".notmuch", "notmuch-config", "new", "cur"])
            (sort files)

          step "send mail and go back to threads"
          sendKeys "y" (Regex ("Query:\\s" <> buildAnsiRegex [] ["34"] [] <> "tag:inbox"))

          -- check that the sent mail can be parsed without errors
          step "parse mail with purebred-email"
          assertMailSuccessfullyParsed (testdir </> "sentMail")

          -- check that the sent mail is indexed
          step "focus query"
          sendKeys ":" (Regex (buildAnsiRegex [] ["37"] [] <> "tag"))

          step "delete all input"
          sendKeys "C-u" (Regex ("Query: " <> buildAnsiRegex [] ["37"] []))

          step "enter sent tags"
          sendLine "tag:sent" (Substring "Draft mail subject")


          -- check that a copy of the sent mail has been copied to our Maildir
          step "Drafts directory is empty"
          assertFileAmountInMaildir (mdir </> "Drafts" </> "new") 0

          step "Sent directory has a new entry"
          assertFileAmountInMaildir (mdir </> "Sent" </> "cur") 1

testSendFailureHandling :: PurebredTestCase
testSendFailureHandling =
  purebredTmuxSession "send failure does not lose mail" $ \step -> do
    mdir <- view envMaildir
    setEnvVarInSession "PUREBRED_SEND_FAIL" "1"
    startApplication
    composeNewMail step

    step "send mail attempt #1 fails"
    sendKeys "y" (Substring "PUREBRED_SEND_FAIL")

    step "compose view remains active"
    assertSubstringS "From: \"Joe Bloggs\" <joe@foo.test>"

    step "Sent folder doesn't exist yet"
    files <- liftIO $ listDirectory mdir
    liftIO $
      assertEqual
      "expected no maildir directories"
      (sort ["Drafts", ".notmuch", "notmuch-config", "new", "cur"])
      (sort files)

    step "send mail attempt #2 succeeds"
    sendKeys "y" (Regex ("Query:\\s" <> buildAnsiRegex [] ["34"] [] <> "tag:inbox"))

    -- check that the sent mail is indexed
    step "focus query"
    sendKeys ":" (Regex (buildAnsiRegex [] ["37"] [] <> "tag"))

    step "delete all input"
    sendKeys "C-u" (Regex ("Query: " <> buildAnsiRegex [] ["37"] []))

    step "enter sent tags"
    sendLine "tag:sent" (Substring "Draft mail subject")

    step "Sent directory has a new entry"
    assertFileAmountInMaildir (mdir </> "Sent" </> "cur") 1

testReplyRendersNonASCIIHeadersCorrectly :: PurebredTestCase
testReplyRendersNonASCIIHeadersCorrectly =
  purebredTmuxSession "reply to msg w/ utf8 From; mailbox renders properly" $ \step -> do
    startApplication
    step "focus search edit"
    sendKeys ":" (Regex (buildAnsiRegex [] ["37"] [] <> "tag"))

    step "delete all input"
    sendKeys "C-u" (Regex ("Query: " <> buildAnsiRegex [] ["37"] []))

    step "search for msg <1234@url>"
    sendLine "id:1234@url" (Substring "Item 1 of 1")

    step "open thread"
    sendKeys "Enter" (Substring "Beginning of large text")

    step "start replying"
    sendKeys "r" (Substring "> Beginning of large text")

    step "exit vim"
    sendLine ": x" (Substring "Attachments") >>= put
    assertRegexS $ T.encodeUtf8 "To: \"Róman Joost\" <roman@bromeco.de>"

testGroupReply :: PurebredTestCase
testGroupReply =
  purebredTmuxSession "group reply Cc's recipients of parent" $ \step -> do
    startApplication
    step "focus search edit"
    sendKeys ":" (Regex (buildAnsiRegex [] ["37"] [] <> "tag"))

    step "delete all input"
    sendKeys "C-u" (Regex ("Query: " <> buildAnsiRegex [] ["37"] []))

    step "search for msg <20170817035004.55C4580B8F@host.example>"
    sendLine "id:20170817035004.55C4580B8F@host.example" (Substring "Item 1 of 1")

    step "open thread"
    sendKeys "Enter" (Substring "This is a test mail for purebred")

    step "start replying"
    sendKeys "g" (Substring "> This is a test mail for purebred")

    step "exit vim"
    sendLine ": x" (Substring "Attachments") >>= put
    assertRegexS $ T.encodeUtf8 "To: frase@host.example"
    assertRegexS $ T.encodeUtf8 "Cc: roman@host.example, joe@host.example"

findMail ::
     ( HasTmuxSession testEnv
     , MonadReader testEnv m
     , MonadState Capture m
     , MonadIO m
     )
  => (String -> m ())
  -> String -- ^ query
  -> m Capture
findMail step query = do
  step ("search for mail with query: " <> query)
  sendKeys ":" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "tag:inbox"))
  sendKeys "C-u" (Regex ("Query: " <> buildAnsiRegex [] ["37"] [] <> "\\s+"))
  step "enter free text search"
  sendLine query (Substring "Item 1 of 1")

assertEditorResetsToInitialValue ::
     HasTmuxSession testEnv
  => (MonadReader testEnv m, MonadState Capture m, MonadIO m) =>
       (String -> m ()) -> String -> B.ByteString -> B.ByteString -> m ()
assertEditorResetsToInitialValue step key focused unfocused = do
    step "focusing editor"
    sendKeys key (Regex $ focused <> "\\s+")

    step "entering bogus characters"
    sendKeys "asdf" (Regex $ focused <> "asdf" <> "\\s+")

    step "abort and expect old editor value is reset"
    sendKeys "Escape" (Regex $ unfocused <> "\\s+") >>= put
    assertConditionS (Not (Substring "Failed reading"))

composeNewMail ::
     HasTmuxSession testEnv
  => (MonadReader testEnv m, MonadState Capture m, MonadIO m) =>
       (String -> m ()) -> m ()
composeNewMail step = do
    step "start composition"
    sendKeys "m" (Substring "From")

    step "accept default"
    sendKeys "Enter" (Substring "To")

    step "enter to: email"
    sendKeys "user@to.test\r" (Substring "Subject")

    step "leave default"
    sendKeys "Draft mail subject\r" (Substring "~")

    step "enter mail body"
    sendKeys "iThis is a test body" (Substring "body")

    step "exit insert mode in vim"
    sendKeys "Escape" (Substring "body")

    step "exit vim"
    sendKeys ": x\r" (Substring "text/plain") >>= put
    assertSubstringS "From: \"Joe Bloggs\" <joe@foo.test>"


parseMail :: B.ByteString -> Either String MIMEMessage
parseMail = parse (message mime)

assertSubstr :: MonadIO m => String -> String -> m ()
assertSubstr needle haystack = liftIO $ assertBool
  (needle <> " not found in\n\n" <> haystack)
  (needle `isInfixOf` haystack)

assertMailSuccessfullyParsed :: (MonadIO m) => String -> m ()
assertMailSuccessfullyParsed fp = do
  contents <- liftIO $ B.readFile fp
  let result = parseMail contents
  liftIO $ assertBool "expected successful MIMEMessage" (isRight result)

assertFileAmountInMaildir :: (MonadIO m) => FilePath -> Int -> m ()
assertFileAmountInMaildir maildir expected =
  let errmsg fs = "expecting " <> show expected <> " file(s), dir contents: " <> show fs
   in liftIO $ do
    -- Wait a bit so we can be sure that the IO operation has
    -- completed. If we don't wait here, the UI has most likely
    -- repainted quicker than the deletion of the file ending in
    -- flakyness. The test will most likely pass quicker on faster IO
    -- machines than in our CI.
    threadDelay 200000  -- 0.2 seconds
    files <- listDirectory maildir
    assertEqual (errmsg files) expected (length files)

-- Global test environment (shared by all test cases)
newtype GlobalEnv = GlobalEnv FilePath

-- Session test environment
data Env = Env
  { _envConfigDir :: FilePath
  , _envMaildir :: FilePath
  , _envNotmuchConfig :: FilePath
  , _envSessionName :: String
  }

instance HasTmuxSession Env where
  tmuxSession = envSessionName

-- | Session-specific config dir
envConfigDir :: Lens' Env FilePath
envConfigDir = lens _envConfigDir (\s b -> s { _envConfigDir = b })

envMaildir :: Lens' Env FilePath
envMaildir = lens _envMaildir (\s b -> s { _envMaildir = b })

envNotmuchConfig :: Lens' Env FilePath
envNotmuchConfig = lens _envNotmuchConfig (\s b -> s { _envNotmuchConfig = b })

envSessionName :: Lens' Env String
envSessionName = lens _envSessionName (\s b -> s { _envSessionName = b })

-- | Tear down a test session
tearDown :: Env -> IO ()
tearDown (Env confdir mdir _ _) = do
  removeDirectoryRecursive confdir
  removeDirectoryRecursive mdir

-- | Set up a test session.
setUp :: GlobalEnv -> TmuxSession -> IO Env
setUp (GlobalEnv globalConfigDir) sessionName = do
  maildir <- setUpTempMaildir
  nmCfg <- setUpNotmuchCfg maildir
  setUpNotmuch nmCfg

  confdir <- mkTempDir
  runProcess_ $ proc "sh" ["-c", "cp -a " <> globalConfigDir <> "/* " <> confdir]

  flip runReaderT sessionName $ do
    -- a) Make the regex less color code dependent by setting the TERM to 'ansi'.
    -- This can happen if different environments support more than 16 colours (e.g.
    -- background values > 37), while our CI environment only supports 16 colours.
    setEnvVarInSession "TERM" "ansi"

    -- set the config dir
    setEnvVarInSession "PUREBRED_CONFIG_DIR" confdir
    setEnvVarInSession "NOTMUCH_CONFIG" nmCfg

  pure $ Env confdir maildir nmCfg sessionName

precompileConfig :: FilePath -> IO ()
precompileConfig testdir = do
  env <- getEnvironment
  let systemEnv = ("PUREBRED_CONFIG_DIR", testdir) : env
      config = setEnv systemEnv $ proc "purebred" ["--version"]
  runProcess_ config

-- | Get the explicitly-specified source directory via SRCDIR
-- env var, or fall back to CWD.
getSourceDirectory :: IO FilePath
getSourceDirectory = lookupEnv "SRCDIR" >>= maybe getCurrentDirectory pure

setUpPurebredConfig :: FilePath -> IO ()
setUpPurebredConfig testdir = do
  c <- getSourceDirectory
  copyFile (c <> "/configs/purebred.hs") (testdir <> "/purebred.hs")

mkTempDir :: IO FilePath
mkTempDir = getCanonicalTemporaryDirectory >>= flip createTempDirectory "purebredtest"

-- | Set up a temporary Maildir containing the test database
-- The returned directory contains the 'Maildir' subdirectory.
setUpTempMaildir :: IO FilePath
setUpTempMaildir = do
  basedir <- mkTempDir
  cwd <- getSourceDirectory
  runProcess_ $ proc "cp" ["-r", cwd <> "/test/data/Maildir/", basedir]
  let mdir = basedir </> "Maildir"

  -- Rename files with maildir flags ; these had to be renamed (':' replaced
  -- with '_') to appease Hackage requirement that tarballs only contain
  -- filenames that are valid on both POSIX and Windows.  We have to fix the
  -- filenames here before using them.
  --
  -- In a Nix system the PATH environment may contain relative paths.
  -- For security reasons find(1) refuses to run when -execdir is given
  -- and PATH contains relative paths.  So we have to remove relative
  -- dirs from PATH.
  --
  path <- intercalate [searchPathSeparator]
          . filter isAbsolute
          <$> getSearchPath
  let
    f (k, _) | k == "PATH" = (k, path)
    f x = x
  env <- fmap f <$> getEnvironment
  runProcess_ $ setEnv env $ proc "find"
    [ mdir, "-name", "*_2,*"
    , "-execdir", "sh", "-c", "mv {} $(echo {} | sed s/_2,/:2,/)", ";"
    ]

  pure mdir

-- | run notmuch to create the notmuch database
-- Note: discard stdout which otherwise clobbers the test output
setUpNotmuch :: FilePath -> IO ()
setUpNotmuch notmuchcfg = void $ readProcess_ $ proc "notmuch" ["--config=" <> notmuchcfg, "new" ]

-- | Write a minimal notmuch config pointing to the given maildir.
-- Returns the path to the notmuch configuration file (which is
-- created under the given maildir directory).
--
setUpNotmuchCfg :: FilePath -> IO FilePath
setUpNotmuchCfg dir = do
  let cfgData = "[database]\npath=" <> dir <> "\n"
      cfgFile = dir <> "/notmuch-config"
  writeFile cfgFile cfgData $> cfgFile

purebredTmuxSession = withTmuxSession setUp tearDown

-- | convenience function to print captured output to STDERR
debugOutput :: String -> IO ()
debugOutput out = do
  d <- lookupEnv "DEBUG"
  when (isJust d) $ hPutStr stderr ("\n\n" <> out)

-- | start the application
-- Note: this is currently defined as an additional test step for no good
-- reason.
startApplication :: (MonadReader Env m, MonadIO m) => m ()
startApplication = do
  srcdir <- liftIO getSourceDirectory
  tmuxSendKeys LiteralKeys ("cd " <> srcdir <> "\r")
  tmuxSendKeys InterpretKeys ("purebred\r")
  void $ waitForCondition (Substring "Purebred: Item") defaultRetries defaultBackoff

-- | A list item which is toggled for a batch operation
--
selectedListItem :: B.ByteString
selectedListItem = buildAnsiRegex [] ["37"] ["43"]

toggledListItem :: B.ByteString
toggledListItem = buildAnsiRegex [] ["36"] []

newListItem :: B.ByteString
newListItem = buildAnsiRegex [] ["37"] ["49"]
