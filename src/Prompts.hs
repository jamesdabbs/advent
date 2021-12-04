{-# LANGUAGE OverloadedStrings #-}

module Prompts
  ( Id
  , initialize
  ) where

import Control.Lens
import Data.ByteString.Lazy (toStrict)
import Data.String (String)
import qualified Data.Text as Text
import Network.Wreq
import Protolude hiding (toStrict)
import System.Directory (createDirectoryIfMissing)
import System.Process (callCommand)

type Year = Int
type Day = Int
type Id = (Year, Day)

initialize :: Id -> IO ()
initialize (year, day) = do
  let dirPath = intercalate "/" ["src", "Problems", "Y" <> show year, "D" <> dayNumber day]
  persistInput year day dirPath
  path <- scaffoldSolution year day dirPath
  callCommand $ "code " <> path
  callCommand $ "open " <> intercalate "/" [uri, show year, "day", show day]

uri :: String
uri = "https://adventofcode.com"

fetchPage :: String -> IO (Either Text Text)
fetchPage path = do
  sessionId <- encodeUtf8 <$> readFile "session"
  let opts = defaults & header "Cookie" .~ ["session=" <> sessionId]
  response <- getWith opts path
  let body = decodeUtf8 $ toStrict $ response ^. responseBody
      status = response ^. responseStatus . statusCode
  return $ if status == 200 then Right body else Left body

fetchInput :: Id -> IO (Either Text Text)
fetchInput (year, day) = fetchPage $ intercalate "/" [uri, show year, "day", show day, "input"]

writeInput :: String -> Text -> IO ()
writeInput dirPath input = do
  createDirectoryIfMissing True dirPath
  writeFile (dirPath <> "/input") input

persistInput :: Year -> Day -> String -> IO ()
persistInput year day dirPath = do
  result <- fetchInput (year, day)
  case result of
    Right input -> do
      writeInput dirPath input
      putStrLn $ "Wrote input to " <> dirPath
    Left err -> die err

scaffoldSolution :: Year -> Day -> String -> IO String
scaffoldSolution year day dirPath = do
  let path = dirPath <> "/Solution.hs"
  writeFile path solution
  return path
  where
    solution = Text.unlines
      [ Text.pack $ "module Problems.Y" <> show year <> ".D" <> dayNumber day <> ".Solution where"
      , ""
      , "import Import"
      , ""
      , "import qualified Solution"
      , ""
      , "solution :: Solution' Input Output Output'"
      , "solution = Solution.basic' parse part1 part2"
      , ""
      , "type Input = ()"
      , "type Output = ()"
      , "type Output' = ()"
      , ""
      , "parse :: Parser Input"
      , "parse = return ()"
      , ""
      , "part1 :: Input -> Output"
      , "part1 input = ()"
      , ""
      , "part2 :: Input -> Output'"
      , "part2 input = ()"
      ]

dayNumber :: Int -> String
dayNumber = Text.unpack . Text.justifyRight 2 '0' . show
