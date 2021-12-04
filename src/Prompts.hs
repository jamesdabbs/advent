{-# LANGUAGE OverloadedStrings #-}

module Prompts
  ( Id
  , fetchInput
  , persistInput
  ) where

import Control.Lens
import Data.ByteString.Lazy (toStrict)
import Data.String (String)
import Network.Wreq
import Protolude hiding (toStrict)
import System.Directory (createDirectoryIfMissing)

type Year = Int
type Day = Int
type Id = (Year, Day)

uri :: String
uri = "https://adventofcode.com"

fetchPage :: String -> IO (Either Status Text)
fetchPage path = do
  sessionId <- encodeUtf8 <$> readFile "session"
  let opts = defaults & header "Cookie" .~ ["session=" <> sessionId]
  response <- getWith opts path
  let body = decodeUtf8 $ toStrict $ response ^. responseBody
      status = response ^. responseStatus
  case status ^. statusCode of
    200 -> return $ Right body
    _ -> return $ Left status

fetchInput :: Id -> IO (Either Status Text)
fetchInput (year, day) = fetchPage $ intercalate "/" [uri, show year, "day", show day, "input"]

writeInput :: Year -> Day -> Text -> IO ()
writeInput year day input = do
  let dirPath = intercalate "/" ["problems", show year, show day]
  createDirectoryIfMissing True dirPath
  writeFile (dirPath <> "/input") input

persistInput :: Year -> Day -> IO ()
persistInput year day = do
  result <- fetchInput (year, day)
  case result of
    Right input -> writeInput year day input
    Left err -> print err
