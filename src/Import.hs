module Import
  ( module X
  ) where

import Protolude as X hiding (Down, check, lines, link, magnitude, note, reduce)

import Control.Arrow as X ((&&&), (>>>), (***))
import Data.Attoparsec.Text as X (Parser, anyChar, choice, decimal, digit, many', many1, notChar, sepBy, sepBy1, signed, takeText)
import Data.List as X (partition)
import Data.Matrix as X (Matrix)
import Data.Maybe as X (fromJust)
import Data.Point as X (Point)
import Data.String as X (String)
import Data.Vector as X (Vector)
import Solution as X (Solution, Solution')
import Utils as X
