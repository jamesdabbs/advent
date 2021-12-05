module Api
  ( fetchInput
  , submitAnswer
  ) where

import Control.Lens hiding (parts)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.String (String)
import Network.Wreq
import Protolude hiding (toStrict)

type Year = Int
type Day = Int
type Id = (Year, Day)

uri :: String
uri = "https://adventofcode.com"

fetchInput :: Id -> IO (String, Either Text Text)
fetchInput (year, day) = do
  let url = path [show year, "day", show day, "input"]
  result <- doGet url
  return (url, result)

submitAnswer :: Int -> Int -> Int -> Text -> IO (Either Text Text)
submitAnswer year day level answer = do
  let url = path [show year, "day", show day, "answer"]
  sessionId <- encodeUtf8 <$> readFile "session"
  let opts = defaults & header "Cookie" .~ ["session=" <> sessionId]
  present <$> postWith opts url ["level" := level, "answer" := answer]

present :: Response Data.ByteString.Lazy.ByteString -> Either Text Text
present response =
  let body = decodeUtf8 $ toStrict $ response ^. responseBody
      status = response ^. responseStatus . statusCode
  in if status == 200 then Right body else Left body

path :: [String] -> String
path parts = intercalate "/" $ uri : parts

doGet :: String -> IO (Either Text Text)
doGet url = do
  sessionId <- encodeUtf8 <$> readFile "session"
  let opts = defaults & header "Cookie" .~ ["session=" <> sessionId]
  present <$> getWith opts url