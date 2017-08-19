{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE OverloadedStrings #-}
module TestUserAcceptance where

import qualified Data.Text as T
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)
import Data.Ini (parseIni, writeIniFileWith, KeySeparator(..), WriteIniSettings(..))
import Data.Semigroup ((<>))
import Control.Concurrent
       (newEmptyMVar, putMVar, takeMVar)
import System.Timeout (timeout)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource
       (register, release, resourceForkIO, runResourceT, ResourceT, ReleaseKey)
import Control.Exception (catch, IOException)
import System.IO (hPutStr, stderr)
import Control.Monad (void)

import System.Process (callProcess, readProcess)
import System.Directory
       (getCurrentDirectory, removeFile, getTemporaryDirectory,
        removeDirectoryRecursive)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsFile)

import Network.Socket hiding (recv)
import Network.Socket
       (bind, socket, Family(..), SocketType(..), defaultProtocol, SockAddr(..))
import Network.Socket.ByteString (recv)
import Data.ByteString.Char8 (pack, ByteString)

systemTests ::
  TestTree
systemTests = testGroup "result in the right state" [testMakesHardcopy]

testMakesHardcopy ::
  TestTree
testMakesHardcopy = goldenVsFile "does not crash" "test/data/test.golden" "/tmp/testoutput" (runResourceT $ tmuxSession steps "purebredtest")
  where steps = [ApplicationStep ["Enter"]]

data ApplicationStep = ApplicationStep [String]

tmuxSession :: [ApplicationStep] -> String -> ResourceT IO ()
tmuxSession xs sessionname = do
    systmp <- liftIO $ getCanonicalTemporaryDirectory
    testdir <- liftIO $ createTempDirectory systmp "purebredtest"
    mdir <-
        liftIO $
        do mdir <- prepareMaildir testdir
           prepareNotmuchCfg testdir mdir >>= prepareNotmuch >> pure mdir
    tmuxRkey <- createTmuxSession sessionname
    startApplication mdir
    liftIO $
        do runSteps xs
           snapshotState sessionname
    release tmuxRkey
    -- only remove the tempdir if the whole session run was without problems,
    -- otherwise it'll help to debug issues
    liftIO $ removeDirectoryRecursive testdir

runSteps :: [ApplicationStep] -> IO ()
runSteps steps = mapM_ (\(ApplicationStep xs) -> callProcess "tmux" (communicateSessionArgs ++ xs)) steps

snapshotState :: String -> IO ()
snapshotState sessionname = do
    systmp <- getCanonicalTemporaryDirectory
    readProcess "tmux" hardcopyArgs [] >>= writeFile (systmp <> "/testoutput")
    where
      hardcopyArgs = ["capture-pane", "-p", "-t", sessionname]

startApplication :: String -> ResourceT IO ()
startApplication testmdir = do
    liftIO $ do callProcess "tmux" (communicateSessionArgs ++ ["-l", "purebred --database " <> testmdir])
                callProcess "tmux" (communicateSessionArgs ++ ["Enter"])
    -- prepare thread waiting for purebred to signal readiness
    baton <- liftIO $ newEmptyMVar
    sockAddr@(SockAddrUnix sfile) <- purebredSocketAddr
    rkey <- register (removeFile sfile)
    _ <- resourceForkIO $ waitReady sockAddr >> do liftIO $ putMVar baton "ready"
    -- purebred should be running by now, send the "special" key for purebred to
    -- connect to the socket and send ready
    liftIO $ do callProcess "tmux" (communicateSessionArgs ++ ["C-t"])
                void $ timeout applicationStartupTimeout $ takeMVar baton
    -- clean up socket file
    release rkey

createTmuxSession :: String -> ResourceT IO ReleaseKey
createTmuxSession sessionname = do
    liftIO $
        callProcess
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
            , "purebred"]
    register (cleanUpTmuxSession sessionname)

cleanUpTmuxSession :: String -> IO ()
cleanUpTmuxSession sessionname = do
    catch
        (callProcess "tmux" ["kill-session", "-t", sessionname])
        (\e ->
              do let err = show (e :: IOException)
                 hPutStr stderr ("Exception when killing session: " ++ err)
                 pure ())

purebredSocketAddr :: ResourceT IO SockAddr
purebredSocketAddr = do
  tmp <- liftIO $ getTemporaryDirectory
  let socketfile = (tmp <> "/purebred.socket")
  pure $ SockAddrUnix socketfile

waitReady :: SockAddr -> ResourceT IO ()
waitReady addr = do
    liftIO $
        do soc <- socket AF_UNIX Datagram defaultProtocol
           bind soc addr
           d <- recv soc 4096
           if d /= applicationReadySignal
               then error "application did not start up in time"
               else close soc

applicationReadySignal :: ByteString
applicationReadySignal = pack "READY=1"

applicationStartupTimeout :: Int
applicationStartupTimeout = 10 ^ 6 * 6

communicateSessionArgs :: [String]
communicateSessionArgs = words "send-keys -t purebredtest"

prepareNotmuch :: FilePath -> IO ()
prepareNotmuch notmuchcfg = callProcess "notmuch" ["--config=" <> notmuchcfg, "new"]

prepareNotmuchCfg :: FilePath -> FilePath -> IO (FilePath)
prepareNotmuchCfg testdir testmdir = do
  let (Right ini) = parseIni (T.pack "[database]\npath=" <> T.pack testmdir)
  let nmcfg = testdir <> "/notmuch-config"
  writeIniFileWith (WriteIniSettings EqualsKeySeparator) nmcfg ini
  pure nmcfg

prepareMaildir :: FilePath -> IO (FilePath)
prepareMaildir testdir = do
  let testmdir = testdir <> "/Maildir/"
  c <- getCurrentDirectory
  let maildir = c <> "/test/data/Maildir/"
  callProcess "cp" ["-r", maildir, testmdir]
  pure testmdir
