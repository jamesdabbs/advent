module Solution
  ( SolutionM(..)
  , Solution
  , solve
  ) where

import Protolude

import Data.Attoparsec.Text (Parser, endOfInput, parseOnly, skipSpace)
import qualified Data.Text as Text

data SolutionM m input output = Solution
  { parser :: Parser input
  , run :: input -> m (output, output)
  }

type Solution = SolutionM IO

solve :: Show output => Solution input output -> Text -> IO ()
solve (Solution parser run) raw =
  case parseOnly (parser <* skipSpace <* endOfInput) raw of
    Left err ->
      die $ "Could not parse input: " <> Text.pack err
    Right input -> do
      output <- run input
      print output
