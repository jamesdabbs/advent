module Parsers
  ( lines
  ) where

import Protolude hiding (lines)
import Data.Attoparsec.Text (Parser, sepBy)

lines :: Parser a -> Parser [a]
lines = flip sepBy "\n"