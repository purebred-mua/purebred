module Storage.Notmuch where

import Notmuch
import Notmuch.Search

import Data.Foldable (toList)
import qualified Data.Vector as Vec


getMessages :: String -> IO (Vec.Vector String)
getMessages dbfp = do
  db' <- databaseOpen dbfp
  case db' of
    Left status -> do
        error $ show status
    Right db -> do
        q <- query db (FreeForm "tag:inbox")
        msgs <- messages q
        hdrs <- (mapM (messageHeader "Subject")) msgs
        return $ Vec.fromList $ toList hdrs
