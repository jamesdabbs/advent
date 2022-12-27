module Problems.Y2022.D12.Solution where

import Algo.Djikstra.Basic (Graph, distance)
import Data.Attoparsec.Text (satisfy)
import qualified Data.Map as Map
import Import
import Parsers (pointGrid)
import qualified Solution

solution :: Solution Input (Maybe Int)
solution = Solution.basic parse part1 part2

type Input = Map Point Char

parse :: Parser Input
parse = pointGrid $ satisfy (/= '\n')

-- 447
part1 :: Input -> Maybe Int
part1 input = do
  (start, _) <- find ((== 'E') . snd) $ Map.toList input
  (end, _) <- find ((== 'S') . snd) $ Map.toList input
  distance (toGraph input) start (== end)

toGraph :: Input -> Graph
toGraph heights = Map.mapWithKey closeNeighbors heights
  where
    closeNeighbors p h = filter (\q -> height q `near` h) $ neighbors p
    height q = Map.findWithDefault '!' q heights
    neighbors (x, y) = filter (`Map.member` heights) [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]

near :: Char -> Char -> Bool
near a b = val b - val a <= 1
  where
    val 'S' = ord 'a'
    val 'E' = ord 'z'
    val c = ord c

-- 446
part2 :: Input -> Maybe Int
part2 input = do
  (start, _) <- find ((== 'E') . snd) $ Map.toList input
  distance (toGraph input) start $ \p -> Map.findWithDefault '!' p input == 'a'
