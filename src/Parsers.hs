module Parsers
  ( lines
  , pointGrid
  , readEnumBy
  , word
  ) where

import Protolude hiding (lines)
import Data.Attoparsec.Text
import qualified Data.Map as Map
import Data.Point (Point)
import qualified Data.Text as Text

lines :: Parser a -> Parser [a]
lines = flip sepBy "\n"

readEnumBy :: (Bounded a, Enum a, Show a) => (Text -> Text) -> Parser a
readEnumBy present = choice $ map p [minBound .. maxBound]
  where
    p v = (string $ present $ Text.pack $ show v) $> v

word :: Parser Text
word = Text.pack <$> many' letter

pointGrid :: Parser a -> Parser (Map Point a)
pointGrid parser = go mempty (0, 0)
  where
    go acc (x, y) = choice
      [ do
          a <- parser
          go (Map.insert (x, y) a acc) (x + 1, y)
      , void "\n" >> go acc (0, y + 1)
      , atEnd >> return acc
      ]
