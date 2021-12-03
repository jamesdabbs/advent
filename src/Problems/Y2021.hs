module Problems.Y2021
  ( solutions
  ) where

import Protolude

import qualified Data.Map as Map

import Solution (solve)

import qualified Problems.Y2021.D01.Solution as D01
import qualified Problems.Y2021.D02.Solution as D02
import qualified Problems.Y2021.D03.Solution as D03
import qualified Problems.Y2021.D04.Solution as D04

solutions :: Map Int (Text -> IO ())
solutions = Map.fromList
  [ (1, solve D01.solution)
  , (2, solve D02.solution)
  , (3, solve D03.solution)
  , (4, solve D04.solution)
  ]