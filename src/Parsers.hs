module Parsers
  ( lines
  , word
  ) where

import Protolude hiding (lines)
import Data.Attoparsec.Text
import qualified Data.Text as Text

lines :: Parser a -> Parser [a]
lines = flip sepBy "\n"

word :: Parser Text
word = Text.pack <$> many' letter
