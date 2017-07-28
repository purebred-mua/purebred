-- | module to integrate with a mail parser. This is needed to actually view the
-- entire mail and it's attachments.
module Storage.ParsedMail where

import           Codec.MIME.Parse    (parseMIMEMessage)
import           Control.Exception   (try)
import           Control.Lens.Getter ((^.))
import qualified Data.Text           as T
import           Data.Text.IO        (readFile)
import           Prelude             hiding (readFile)
import           Storage.Mail        (Mail, filepath)
import           UI.Types

parseMail :: Mail -> IO (Either String ParsedMail)
parseMail m = do
  msg <- try (readFile $ m^.filepath) :: IO (Either IOError T.Text)
  case msg of
    Left e -> pure $ Left $ show e
    Right contents -> pure $ Right $ MIMEMail (parseMIMEMessage contents)
