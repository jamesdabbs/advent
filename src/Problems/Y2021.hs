module Problems.Y2021
  ( solutions
  ) where

import Protolude

import qualified Data.Map as Map

import Solution (solve)

import qualified Problems.Y2021.D01.Solution as D01
import qualified Problems.Y2021.D02.Solution as D02

solutions :: Map Int (Text -> IO ())
solutions = Map.fromList
  [ (1, solve D01.solution)
  , (2, solve D02.solution)
  ]