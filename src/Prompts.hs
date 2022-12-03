module Prompts
  ( Id
  , initialize
  , persistPrompt
  ) where

import Api (Prompt(..), fetchInput, fetchPrompt)
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
  persistInput year day
  let d = dirPath year day
  putStrLn $ "Wrote input to " <> d
  persistPrompt year day >>= \case
    Left err -> die err
    Right prompt -> do
      putStrLn $ part1 prompt
      scaffoldSolution year day
      callCommand $ intercalate " " ["code", d <> "/input", d <> "/prompt", d <> "/Solution.hs"]

persistPrompt :: Year -> Day -> IO (Either Text Prompt)
persistPrompt year day = fetchPrompt year day >>= \case
  Left err -> return $ Left err
  Right prompt -> do
    let promptPath = dirPath year day <> "/prompt"
    writeFile promptPath $ renderPrompt prompt
    return $ Right prompt

writeInput :: Year -> Day -> Text -> IO ()
writeInput year day input = do
  createDirectoryIfMissing True $ dirPath year day
  writeFile (dirPath year day <> "/input") input

persistInput :: Year -> Day -> IO String
persistInput year day = do
  (url, result) <- fetchInput year day
  case result of
    Right input -> do
      writeInput year day input
      return url
    Left err -> die err

scaffoldSolution :: Year -> Day -> IO String
scaffoldSolution year day = do
  let path = dirPath year day <> "/Solution.hs"
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
      , "import qualified Data.Map as Map"
      , "import qualified Data.Text as Text"
      , "import qualified Data.Set as Set"
      , "import qualified Solution"
      , ""
      , "solution :: Solution' Input Output Output'"
      , "solution = Solution.basic parse part1 part2"
      , ""
      , "type Input = ()"
      , "type Output = ()"
      , "type Output' = Text"
      , ""
      , "parse :: Parser Input"
      , "parse = return ()"
      , ""
      , "part1 :: Input -> Output"
      , "part1 input = ()"
      , ""
      , "part2 :: Input -> Output'"
      , "part2 input = show input"
      ]

renderPrompt :: Prompt -> Text
renderPrompt Prompt{..} = Text.unlines $
  [ "#" <> title
  , ""
  , "## Part 1"
  , ""
  , part1
  ] <> p2
  where
    p2 = case part2 of
      Just body -> ["## Part 2", "", body]
      _ -> []

dirPath :: Year -> Day -> String
dirPath year day = intercalate "/" ["src", "Problems", "Y" <> show year, "D" <> Text.unpack (dayNumber day)]
