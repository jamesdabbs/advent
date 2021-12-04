module Import
  ( module X
  ) where

import Protolude as X hiding (lines)

import Control.Arrow as X ((&&&), (>>>))
import Control.Monad as X (join, replicateM)
import Data.Attoparsec.Text as X (Parser, choice, decimal, many1, sepBy, sepBy1)
import Data.Matrix as X (Matrix)
import Data.Vector as X (Vector)
import Solution as X (SolutionM(Solution), Solution)
import Utils as X (count, lines)
