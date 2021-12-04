module Solution
  ( Solution
  , basic
  , solve
  ) where

import Protolude

import Control.Arrow ((&&&))
import Data.Attoparsec.Text (Parser, endOfInput, parseOnly, skipSpace)
import qualified Data.Text as Text

data SolutionM m input output = Solution
  { parser :: Parser input
  , run :: input -> m (output, output)
  }

type Solution = SolutionM IO

solve :: Show output => Solution input output -> Text -> IO ()
solve Solution{..} raw =
  case parseOnly (parser <* skipSpace <* endOfInput) raw of
    Left err ->
      die $ "Could not parse input: " <> Text.pack err
    Right input -> do
      output <- run input
      print output

basic :: Parser i -> (i -> o) -> (i -> o) -> Solution i o
basic parser part1 part2 = Solution parser $ pure . (part1 &&& part2)
