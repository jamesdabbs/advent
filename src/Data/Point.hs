module Data.Point
  ( Point(..)
  , move
  , neighbors
  , origin
  ) where

import Protolude
import Data.Direction (Direction(..))

type Point = (Int, Int)

origin :: Point
origin = (0, 0)

move :: Point -> Direction -> Point
move (x, y) N = (x, y + 1)
move (x, y) E = (x + 1, y)
move (x, y) S = (x, y - 1)
move (x, y) W = (x - 1, y)

neighbors :: Point -> [Point]
neighbors (x, y) = do
  x' <- [x - 1 .. x + 1]
  y' <- [y - 1 .. y + 1]
  guard $ (x, y) /= (x', y')
  return (x', y')
