-- | module to integrate with a mail parser. This is needed to actually view the
-- entire mail and it's attachments.
module Storage.ParsedMail where

import           Codec.MIME.Parse    (parseMIMEMessage)
import           Codec.MIME.Type     (MIMEValue)
import           Control.Exception   (try)
import           Control.Lens.Getter ((^.))
import qualified Data.Text           as T
import           Data.Text.IO        (readFile)
import           Prelude             hiding (readFile)
import           Storage.Mail        (Mail, filepath)


type Body = T.Text
type Header = T.Text

-- | a parsed email representing either a MIME or RFC2822 e-mail. Note: RFC2822
-- is currently not implemented, but we're using the same type for the case we
-- add support for it
data ParsedMail
    = MIMEMail MIMEValue
    | RFC2822 [Header]
              Body
    deriving (Show,Eq)

parseMail :: Mail -> IO (Either String ParsedMail)
parseMail m = do
  msg <- try (readFile $ m^.filepath) :: IO (Either IOError T.Text)
  case msg of
    Left e -> pure $ Left $ show e
    Right contents -> pure $ Right $ MIMEMail (parseMIMEMessage contents)
