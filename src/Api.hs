module Api
  ( fetchInput
  , fetchPrompt
  , submitAnswer
  ) where

import Control.Lens hiding (parts)
import Data.ByteString.Lazy (ByteString, toStrict)
import qualified Data.Text as Text
import Data.String (String)
import Network.Wreq
import Protolude hiding (toStrict)
import Text.HTML.TagSoup
import Utils (mapRight)

type Year = Int
type Day = Int
type Id = (Year, Day)

uri :: String
uri = "https://adventofcode.com"

fetchInput :: Year -> Day -> IO (String, Either Text Text)
fetchInput year day = do
  let url = path [show year, "day", show day]
  result <- doGet $ url <> "/input"
  return (url, result)

fetchPrompt :: Year -> Day -> IO (Either Text Text)
fetchPrompt year day = do
  let url = path [show year, "day", show day]
  response <- doGet url
  return $ mapRight parse response

submitAnswer :: Int -> Int -> Int -> Text -> IO (Either Text Text)
submitAnswer year day level answer = do
  putStrLn $ "\n=> Submitting " <> answer
  let url = path [show year, "day", show day, "answer"]
  sessionId <- encodeUtf8 <$> readFile "session"
  let opts = defaults & header "Cookie" .~ ["session=" <> sessionId]
  response <- postWith opts url ["level" := level, "answer" := answer]
  return $ mapRight parse $ present response

present :: Response Data.ByteString.Lazy.ByteString -> Either Text Text
present response =
  let body = decodeUtf8 $ toStrict $ response ^. responseBody
      status = response ^. responseStatus . statusCode
  in if status == 200 then Right body else Left (parse body)

parse :: Text -> Text
parse = fst . Text.breakOn "\n\n\n\n" . innerText . dropWhile (~/= ("<article>" :: String)) . parseTags

path :: [String] -> String
path parts = intercalate "/" $ uri : parts

doGet :: String -> IO (Either Text Text)
doGet url = do
  sessionId <- encodeUtf8 <$> readFile "session"
  let opts = defaults & header "Cookie" .~ ["session=" <> sessionId]
  present <$> getWith opts url
