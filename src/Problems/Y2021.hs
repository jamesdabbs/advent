module Problems.Y2021
  ( solutions
  ) where

import Protolude

import qualified Data.Map as Map

import Solution (solve)

import qualified Problems.Y2021.D01.Solution as D01

solutions :: Map Int (Text -> IO ())
solutions = Map.fromList
  [ (1, solve D01.solution)
  ]