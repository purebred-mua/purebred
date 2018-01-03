{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

{-# LANGUAGE OverloadedStrings #-}

module TestUserAcceptance where

import Data.Char (isAscii, isAlphaNum)
import qualified Data.Text as T
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Data.Functor (($>))
import Data.Ini (parseIni, writeIniFileWith, KeySeparator(..), WriteIniSettings(..))
import Data.Semigroup ((<>))
import Control.Concurrent (threadDelay)
import Control.Exception (catch, IOException)
import System.IO (hPutStr, hPutStrLn, stderr)
import System.Environment (lookupEnv)
import Control.Monad (void, when)
import Data.Maybe (isJust)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT, ask, ReaderT)

import Control.Lens (Lens', view)
import Data.List (isInfixOf, intercalate)
import System.Process (callProcess, readProcess)
import System.Directory
       (getCurrentDirectory, removeDirectoryRecursive, removeFile)
import Test.Tasty (TestTree, TestName, testGroup, withResource)
import Test.Tasty.HUnit (testCaseSteps, assertBool)
import Text.Regex.Posix ((=~))

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

-- | A condition to check for in the output of the program
data Condition
  = Literal String
  | Regex String
  deriving (Show)

systemTests ::
  TestTree
systemTests =
  withResource pre post $ \_ ->
    testGroup "user acceptance tests" $ zipWith ($) tests [0..]
  where
    pre = let n = "keepalive" in setUpTmuxSession n $> n
    post = cleanUpTmuxSession
    tests =
      [ testUserViewsMailSuccessfully
      , testUserCanManipulateNMQuery
      , testUserCanSwitchBackToIndex
      , testCanToggleHeaders
      , testSetsMailToRead
      , testErrorHandling
      , testHelp
      , testManageTagsOnMails
      , testManageTagsOnThreads
      ]

testManageTagsOnMails :: Int -> TestTree
testManageTagsOnMails = withTmuxSession "manage tags on mails" $
  \step -> do
    startApplication

    liftIO $ step "view mail in thread"
    sendKeys "Enter" (Literal "Testmail")

    liftIO $ step "focus command to show mail tags"
    sendKeys "`" (Regex (buildAnsiRegex [] ["37"] ["40"] <> "inbox"))

    liftIO $ step "delete all input"
    sendKeys "C-u" (Regex (buildAnsiRegex [] ["37"] ["40"]))

    liftIO $ step "enter new tag"
    _ <- sendLiteralKeys "inbox, foo, bar ,test"

    liftIO $ step "apply"
    sendKeys "Enter" (Literal "foo bar test")

    liftIO $ step "go back to list of threads"
    sendKeys "Escape" (Literal "Testmail")

    -- find newly tagged mail
    liftIO $ step "focus tag search"
    sendKeys ":" (Regex (buildAnsiRegex [] ["37"] ["40"] <> "tag"))
    sendKeys "C-u" (Regex (buildAnsiRegex [] ["37"] ["40"]))

    liftIO $ step "enter tag to search `foo and bar`"
    _ <- sendLiteralKeys "tag:foo and tag:bar"

    liftIO $ step "apply"
    sendKeys "Enter" (Literal "tag:foo and tag:bar")

    liftIO $ step "view mail in thread"
    sendKeys "Enter" (Literal "Testmail")

    liftIO $ step "attempt to add a new tag"
    sendKeys "`" (Regex (buildAnsiRegex [] ["37"] ["40"] <> "bar,foo"))

    liftIO $ step "cancel tagging and expect old search restored"
    sendKeys "Escape" (Literal "tag:foo and tag:bar")

    pure ()

testManageTagsOnThreads :: Int -> TestTree
testManageTagsOnThreads = withTmuxSession "manage tags on threads" $
  \step -> do
    startApplication

    liftIO $ step "focus command to show thread tags"
    sendKeys "`" (Regex (buildAnsiRegex [] ["37"] ["40"] <> "inbox,unread"))

    liftIO $ step "delete all input"
    sendKeys "C-u" (Regex (buildAnsiRegex [] ["37"] ["40"]))

    liftIO $ step "enter new tag"
    _ <- sendLiteralKeys "thread,only"

    liftIO $ step "apply"
    sendKeys "Enter" (Literal "thread only")

    liftIO $ step "view mails to have same tags"
    sendKeys "Enter" (Literal "only thread")

    pure ()

testHelp :: Int -> TestTree
testHelp = withTmuxSession "help view" $
  \step -> do
    startApplication

    liftIO $ step "shows Keybindings"
    sendKeys "?" (Literal "quit the application")

    sendKeys "Escape" (Literal "Purebred")
    pure ()

testErrorHandling :: Int -> TestTree
testErrorHandling = withTmuxSession "error handling" $
  \step -> do
    startApplication

    testmdir <- getTestMaildir
    liftIO $ removeFile (testmdir <> "/new/1502941827.R15455991756849358775.url")

    liftIO $ step "open thread"
    sendKeys "Enter" (Literal "Testmail")

    liftIO $ step "shows error message"
    sendKeys "Enter" (Literal "FileReadError")
      >>= assertRegex "open(Binary)?File:.*does not exist"

    liftIO $ step "error is cleared with next registered keybinding"
    sendKeys "Up" (Literal "Purebred: Item 1 of 1")

    pure ()

testSetsMailToRead :: Int -> TestTree
testSetsMailToRead = withTmuxSession "user can toggle read tag" $
  \step -> do
    startApplication

    liftIO $ step "open thread"
    sendKeys "Enter" (Literal "This is a test mail for purebred")

    liftIO $ step "first unread mail is opened"
    sendKeys "Escape" (Literal "BrowseMail")
      >>= assertRegex (buildAnsiRegex [] ["37"] ["43"] <> ".*Testmail")

    liftIO $ step "toggle it back to unread (bold again)"
    sendKeys "t" (Regex (buildAnsiRegex ["1"] ["37"] ["43"] <> ".*Testmail"))
    pure ()

testCanToggleHeaders :: Int -> TestTree
testCanToggleHeaders = withTmuxSession "user can toggle Headers" $
  \step -> do
    startApplication
    liftIO $ step "open thread"
    sendKeys "Enter" (Literal "Testmail")

    liftIO $ step "view mail"
    sendKeys "Enter" (Literal "This is a test mail")

    liftIO $ step "toggle to show all headers"
    sendKeys "h" (Regex "[Rr]eturn-[Pp]ath")

    liftIO $ step "toggle filtered headers"
    out <- sendKeys "h" (Literal "This is a test mail")
    assertRegex "Purebred.*\n.*[Ff]rom" out

testUserViewsMailSuccessfully :: Int -> TestTree
testUserViewsMailSuccessfully = withTmuxSession "user can view mail" $
  \step -> do
    startApplication
    liftIO $ step "shows tag"
    out <- capture
    assertSubstrInOutput "inbox" out

    liftIO $ step "open thread"
    sendKeys "Enter" (Literal "Testmail")

    liftIO $ step "view mail"
    sendKeys "Enter" (Literal "This is a test mail")
    pure ()

testUserCanManipulateNMQuery :: Int -> TestTree
testUserCanManipulateNMQuery =
    withTmuxSession
        "manipulating notmuch search query results in empty index" $
        \step -> do
          startApplication
          liftIO $ step "focus command"
          sendKeys ":" (Regex (buildAnsiRegex [] ["37"] ["40"] <> "tag"))

          liftIO $ step "delete all input"
          sendKeys "C-u" (Regex (buildAnsiRegex [] ["37"] ["40"]))

          liftIO $ step "search for non existing tags yielding no results"
          _ <- sendLiteralKeys "does not match anything"
          sendKeys "Enter" (Literal "No items")

          liftIO $ step "search for mail correctly tagged"
          sendKeys ":" (Regex (buildAnsiRegex [] ["37"] ["40"] <> "does"))
          sendKeys "C-u" (Regex (buildAnsiRegex [] ["37"] ["40"]))

          liftIO $ step "enter new tag"
          _ <- sendLiteralKeys "tag:replied"

          liftIO $ step "apply"
          sendKeys "Enter" (Literal "Item 1 of 1")

          liftIO $ step "open thread"
          sendKeys "Enter" (Literal "This is Purebred")

          liftIO $ step "view currently selected mail"
          sendKeys "Enter" (Literal "HOLY PUREBRED")
          pure ()

testUserCanSwitchBackToIndex :: Int -> TestTree
testUserCanSwitchBackToIndex =
  withTmuxSession "user can switch back to mail index during composition" $
        \step -> do
            startApplication
            liftIO $ step "start composition"
            sendKeys "m" (Literal "From")

            liftIO $ step "enter from email"
            sendKeys "testuser@foo.test\r" (Literal "To")

            liftIO $ step "enter to: email"
            sendKeys "user@to.test\r" (Literal "Subject")

            liftIO $ step "enter subject"
            sendKeys "test subject\r" (Literal "~")

            liftIO $ step "enter mail body"
            sendKeys "iThis is a test body" (Literal "body")

            liftIO $ step "exit insert mode in vim"
            sendKeys "Escape" (Literal "body")

            liftIO $ step "exit vim"
            sendKeys ": x\r" (Literal "Attachments")

            liftIO $ step "switch back to index"
            sendKeys "Tab" (Literal "Testmail")

            liftIO $ step "switch back to the compose editor"
            sendKeys "Tab" (Literal "test subject")

            liftIO $ step "cycle to next input field"
            sendKeys "C-n" (Regex (buildAnsiRegex [] ["39"] ["49"] <> "To:\\s+"
                                   <> buildAnsiRegex [] ["37"] ["40"] <> "user@to.test"))
            pure ()

assertSubstrInOutput :: String -> String -> ReaderT a IO ()
assertSubstrInOutput substr out = liftIO $ assertBool (substr <> " not found in\n\n" <> out) $ substr `isInfixOf` out

assertRegex :: String -> String -> ReaderT a IO ()
assertRegex regex out = liftIO $ assertBool
  (show regex <> " does not match out\n\n" <> out
    <> "\n\n raw:\n\n" <> show out)
  (out =~ regex)

sessionNamePrefix :: String
sessionNamePrefix = "purebredtest"

data Env = Env
  { _envDir :: FilePath
  , _envMaildir :: FilePath
  , _envSessionName :: String
  }

envDir :: Lens' Env FilePath
envDir f (Env a b c) = fmap (\a' -> Env a' b c) (f a)

envMaildir :: Lens' Env FilePath
envMaildir f (Env a b c) = fmap (\b' -> Env a b' c) (f b)

envSessionName :: Lens' Env String
envSessionName f (Env a b c) = fmap (\c' -> Env a b c') (f c)
{-# ANN envSessionName ("HLint: ignore Avoid lambda" :: String) #-}

-- | Tear down a test session
tearDown :: Env -> IO ()
tearDown (Env testdir _ sessionName) = do
  removeDirectoryRecursive testdir
  cleanUpTmuxSession sessionName

-- | Set up a test session.
setUp :: Int -> String -> IO Env
setUp i desc = do
  let
    sessionName = intercalate "-" (sessionNamePrefix : show i : descWords)
    descWords = words $ filter (\c -> isAscii c && (isAlphaNum c || c == ' ')) desc
  setUpTmuxSession sessionName
  (testdir, maildir) <- setUpTempMaildir
  pure $ Env testdir maildir sessionName

setUpTempMaildir :: IO (String, String)
setUpTempMaildir = do
  systmp <- getCanonicalTemporaryDirectory
  testdir <- createTempDirectory systmp sessionNamePrefix
  mdir <- setUpMaildir testdir
  setUpNotmuchCfg testdir mdir >>= setUpNotmuch >> pure (testdir, mdir)

-- | run notmuch to create the notmuch database
-- Note: discard stdout which otherwise clobbers the test output
setUpNotmuch :: FilePath -> IO ()
setUpNotmuch notmuchcfg = void $ readProcess "notmuch" ["--config=" <> notmuchcfg, "new"] []

-- | write a notmuch config
-- Note: currently writes a minimal config pointing to our database
setUpNotmuchCfg :: FilePath -> FilePath -> IO FilePath
setUpNotmuchCfg testdir testmdir = do
  let (Right ini) = parseIni (T.pack "[database]\npath=" <> T.pack testmdir)
  let nmcfg = testdir <> "/notmuch-config"
  writeIniFileWith (WriteIniSettings EqualsKeySeparator) nmcfg ini
  pure nmcfg

-- | setup a temporary Maildir for notmuch and the test session
setUpMaildir :: FilePath -> IO FilePath
setUpMaildir testdir = do
  let testmdir = testdir <> "/Maildir/"
  c <- getCurrentDirectory
  let maildir = c <> "/test/data/Maildir/"
  callProcess "cp" ["-r", maildir, testmdir]
  pure testmdir

-- | create a tmux session running in the background
-- Note: the width and height are the default values tmux uses, but I thought
-- it's better to be explicit.
setUpTmuxSession :: String -> IO ()
setUpTmuxSession sessionname =
    catch
        (callProcess
             "tmux"
             [ "new-session"
             , "-x"
             , "80"
             , "-y"
             , "24"
             , "-d"
             , "-s"
             , sessionname
             , "-n"
             , "purebred"])
        (\e ->
              do let err = show (e :: IOException)
                 hPutStrLn stderr ("\nException during setUp: " <> err)
                 pure ())

-- | Kills the whole session including pane and application
cleanUpTmuxSession :: String -> IO ()
cleanUpTmuxSession sessionname =
    catch
        (callProcess "tmux" ["kill-session", "-t", sessionname])
        (\e ->
              do let err = show (e :: IOException)
                 hPutStrLn stderr ("\nException when killing session: " <> err)
                 pure ())


-- | Run all application steps in a session defined by session name.
withTmuxSession
  :: TestName
  -> ((String -> IO ()) -> ReaderT Env IO ())
  -> Int  -- ^ session sequence number (will be appended to session name)
  -> TestTree
withTmuxSession tcname testfx i =
  withResource (setUp i tcname) tearDown $
      \env -> testCaseSteps tcname $ \stepfx -> env >>= runReaderT (testfx stepfx)

-- | Send keys into the program and wait for the condition to be
-- met, failing the test if the condition is not met after some
-- time.
sendKeys :: String -> Condition -> ReaderT Env IO String
sendKeys keys expect = do
    sessionName <- getSessionName
    liftIO $ callProcess "tmux" $ communicateSessionArgs sessionName keys False
    waitForCondition expect defaultCountdown

sendLiteralKeys :: String -> ReaderT Env IO String
sendLiteralKeys keys = do
    sessionName <- getSessionName
    liftIO $ callProcess "tmux" $ communicateSessionArgs sessionName keys True
    waitForString keys defaultCountdown

capture :: ReaderT Env IO String
capture = do
  sessionname <- getSessionName
  liftIO $ readProcess "tmux" ["capture-pane", "-e", "-p", "-t", sessionname] []

getSessionName :: (Monad m) => ReaderT Env m String
getSessionName = view (envSessionName . ask)

getTestMaildir :: (Monad m) => ReaderT Env m FilePath
getTestMaildir = view (envMaildir . ask)

holdOffTime :: Int
holdOffTime = 10 ^ (6 :: Int)

-- | convenience function to print captured output to STDERR
debugOutput :: String -> IO ()
debugOutput out = do
  d <- lookupEnv "DEBUG"
  when (isJust d) $ hPutStr stderr ("\n\n" <> out)

-- | wait for the application to render a new interface which we determine with
--   a given condition. We check up to @n@ times, waiting a short duration
--   between each check, and failing if the tries exhaust with the condition
--   not met.
waitForCondition :: Condition -> Int -> ReaderT Env IO String
waitForCondition cond n = do
  out <- capture >>= checkPane
  liftIO $ assertBool
    ( "Wait time exceeded. Condition not met: '" <> show cond
      <> "' last screen shot:\n\n " <> out <> "\n\n" <> " raw: " <> show out )
    (checkCondition cond out)
  pure out
  where
    checkPane :: String -> ReaderT Env IO String
    checkPane out
      | checkCondition cond out = pure out
      | n <= 0 = pure out
      | otherwise = do
          liftIO $ threadDelay holdOffTime
          waitForCondition cond (n - 1)

checkCondition :: Condition -> String -> Bool
checkCondition (Literal s) = (s `isInfixOf`)
checkCondition (Regex re) = (=~ re)

-- | Convenience version of 'waitForCondition' that checks for a
-- literal string.
--
waitForString :: String -> Int -> ReaderT Env IO String
waitForString = waitForCondition . Literal

defaultCountdown :: Int
defaultCountdown = 5

-- | start the application
-- Note: this is currently defined as an additional test step for no good
-- reason.
startApplication :: ReaderT Env IO ()
startApplication = do
  testmdir <- getTestMaildir
  sessionName <- getSessionName
  liftIO $ callProcess "tmux" $
    communicateSessionArgs sessionName ("purebred --database " <> testmdir <> "\r") False
  void $ waitForString "Purebred: Item" defaultCountdown

communicateSessionArgs
  :: String -- ^ session name
  -> String -- ^ keys
  -> Bool   -- ^ send the keys literally
  -> [String]
communicateSessionArgs sessionName keys asLiteral =
  ["send-keys", "-t", sessionName] <> ["-l" | asLiteral] <> [keys]


type AnsiAttrParam = String
type AnsiFGParam = String
type AnsiBGParam = String

-- | Generate a regex for an escape sequence setting the given
-- foreground and background parameters
--
-- tmux < 03d01eabb5c5227f56b6b44d04964c1328802628 (first released
-- in tmux-2.5) ran attributes, foreground colour and background
-- colour params separated by semicolons (foreground first).
--
-- After that commit, attributes, foreground colours and background
-- colours are written in separate escape sequences.  Therefore for
-- compatibility with different versions of tmux there are two
-- patterns to check.
--
buildAnsiRegex :: [AnsiAttrParam] -> [AnsiFGParam] -> [AnsiBGParam] -> String
buildAnsiRegex attrs fgs bgs =
  let
    withSemis = intercalate ";"
    wrap [] = ""
    wrap xs = "\ESC\\[" <> withSemis xs <> "m"
    tmux24 = wrap (attrs <> fgs <> bgs)
    tmux25 = wrap attrs <> wrap fgs <> wrap bgs
    choice "" "" = ""
    choice "" r = r
    choice l "" = l
    choice l r = "(" <> l <> "|" <> r <> ")"
  in
    choice tmux24 tmux25
