module Parsers
  ( lines
  , readEnumBy
  , word
  ) where

import Protolude hiding (lines)
import Data.Attoparsec.Text
import Data.String (String)
import qualified Data.Text as Text

lines :: Parser a -> Parser [a]
lines = flip sepBy "\n"

readEnumBy :: (Bounded a, Enum a, Show a) => (Text -> Text) -> Parser a
readEnumBy present = choice $ map p [minBound .. maxBound]
  where
    p v = (string $ present $ Text.pack $ show v) $> v

word :: Parser Text
word = Text.pack <$> many' letter
