module Import
  ( module X
  ) where

import Protolude as X hiding (Down, check, lines)

import Control.Arrow as X ((&&&), (>>>))
import Data.Attoparsec.Text as X (Parser, choice, decimal, digit, many', many1, sepBy, sepBy1)
import Data.List as X (partition)
import Data.Matrix as X (Matrix)
import Data.Vector as X (Vector)
import Solution as X (Solution)
import Utils as X (count)
