module Prompts
  ( Id
  , initialize
  ) where

import Api (fetchInput)
import Data.String (String)
import qualified Data.Text as Text
import Protolude hiding (toStrict)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Process (callCommand)
import Utils (dayNumber)

type Year = Int
type Day = Int
type Id = (Year, Day)

initialize :: Id -> IO ()
initialize (year, day) = do
  let dirPath = intercalate "/" ["src", "Problems", "Y" <> show year, "D" <> Text.unpack (dayNumber day)]
  url <- persistInput year day dirPath
  path <- scaffoldSolution year day dirPath
  callCommand $ "code " <> path
  callCommand $ "open " <> url

writeInput :: String -> Text -> IO ()
writeInput dirPath input = do
  createDirectoryIfMissing True dirPath
  writeFile (dirPath <> "/input") input

persistInput :: Year -> Day -> String -> IO String
persistInput year day dirPath = do
  (url, result) <- fetchInput (year, day)
  case result of
    Right input -> do
      writeInput dirPath input
      putStrLn $ "Wrote input to " <> dirPath
      return url
    Left err -> die err

scaffoldSolution :: Year -> Day -> String -> IO String
scaffoldSolution year day dirPath = do
  let path = dirPath <> "/Solution.hs"
  exists <- doesFileExist path
  if exists
    then putStrLn $ "Solution already exists: " <> path
    else do
      putStrLn $ "Writing solution file: " <> path
      writeFile path solution
  return path
  where
    solution = Text.unlines
      [ Text.pack $ "module Problems.Y" <> show year <> ".D" <> Text.unpack (dayNumber day) <> ".Solution where"
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
      , "type Output' = Output"
      , ""
      , "parse :: Parser Input"
      , "parse = return ()"
      , ""
      , "part1 :: Input -> Output"
      , "part1 input = ()"
      , ""
      , "part2 :: Input -> Output'"
      , "part2 input = part1 input"
      ]
