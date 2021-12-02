module Utils
  ( count
  , lines
  ) where

import Protolude hiding (lines)
import Data.Attoparsec.Text (Parser, sepBy)

count :: (a -> Bool) -> [a] -> Int
count condition = length . filter condition

lines :: Parser a -> Parser [a]
lines = flip sepBy "\n"
