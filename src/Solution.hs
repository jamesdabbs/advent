module Solution
  ( Solution
  , Solution'
  , basic
  , basic'
  , solve
  ) where

import Protolude hiding (handle)

import Control.Arrow ((&&&))
import Data.Attoparsec.Text (Parser, IResult(..), parse, skipSpace)
import qualified Data.Text as Text

data SolutionM m input part1 part2 = Solution
  { parser :: Parser input
  , run :: input -> m (part1, part2)
  }

type Solution' i o1 o2 = SolutionM IO i o1 o2
type Solution i o = Solution' i o o

solve :: (Show i, Show o1, Show o2) => Solution' i o1 o2 -> Text -> IO ()
solve Solution{..} = handle . parse (parser <* skipSpace)
  where
    handle (Done "" parsed) = do
      (p1, p2) <- run parsed
      putStrLn ("1. " <> show p1 :: Text)
      putStrLn ("2. " <> show p2 :: Text)
    handle (Partial cont) = handle $ cont ""
    handle (Fail input _ err) = do
      putStrLn $ Text.unlines
        [ "Remaining input"
        , summarize input
        , ""
        , "Parse error"
        , Text.pack err
        ]
      exitWith $ ExitFailure 1
    handle (Done input parsed) = do
      putStrLn $ Text.unlines
        [ "Parsed"
        , show parsed
        , ""
        , "Unconsumed input"
        , summarize input
        ]
      exitWith $ ExitFailure 1

    summarize input = case Text.breakOn "\n" input of
      (pre, "") -> pre
      (pre, _) -> pre <> " ..."

basic' :: Parser i -> (i -> o1) -> (i -> o2) -> Solution' i o1 o2
basic' parser part1 part2 = Solution parser $ pure . (part1 &&& part2)

basic :: Parser i -> (i -> o) -> (i -> o) -> Solution i o
basic = basic'
