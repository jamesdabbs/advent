module Solution
  ( Solution
  , Solution'
  , basic
  , basic'
  , solve
  ) where

import Protolude hiding (handle, toStrict)

import Control.Arrow ((&&&))
import Data.Aeson (ToJSON, encode)
import Data.Attoparsec.Text (Parser, IResult(..), parse, skipSpace)
import Data.ByteString.Lazy (toStrict)
import qualified Data.Text as Text

data SolutionM m input part1 part2 = Solution
  { parser :: Parser input
  , run :: input -> m (part1, part2)
  }

type Solution' i o1 o2 = SolutionM IO i o1 o2
type Solution i o = Solution' i o o

solve :: (Show i, Show o1, Show o2, ToJSON o1, ToJSON o2)
      => Solution' i o1 o2
      -> Text
      -> IO (Text, Text)
solve Solution{..} = handle . parse (parser <* skipSpace)
  where
    handle (Done "" parsed) = do
      (p1, p2) <- run parsed
      putStrLn ("1. " <> encode p1)
      putStrLn ("2. " <> encode p2)
      return (decodeUtf8 . toStrict $ encode p1, decodeUtf8 . toStrict $ encode p2)
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
