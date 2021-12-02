module Import
  ( module X
  ) where

import Protolude as X hiding (lines)
import Data.Attoparsec.Text as X (Parser, choice, decimal, sepBy)
import Solution as X (SolutionM(Solution), Solution)
import Utils as X (count, lines)
