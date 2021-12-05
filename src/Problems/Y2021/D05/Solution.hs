module Problems.Y2021.D05.Solution where

import Import

import qualified Data.Map as Map
import qualified Solution

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

type Input = [(Point, Point)]

parse :: Parser Input
parse = lineSpec `sepBy` "\n"
  where
    pt = (,) <$> decimal <* "," <*> decimal
    lineSpec = (,) <$> pt <* " -> " <*> pt

part1 :: Input -> Int
part1 = filter horizontalOrVertical >>> countCovers

part2 :: Input -> Int
part2 = countCovers

countCovers :: Input -> Int
countCovers = Map.size . Map.filter (>= 2) . foldl f mempty
  where
    f acc = foldr tally acc . connect

    tally :: Point -> Map Point Int -> Map Point Int
    tally = Map.alter (Just . maybe 1 (+1))

horizontalOrVertical :: (Point, Point) -> Bool
horizontalOrVertical ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

connect :: (Point, Point) -> [Point]
connect ((x1, y1), (x2, y2)) =
  let dx = sign $ x2 - x1
      dy = sign $ y2 - y1
  in [(x1 + n * dx, y1 + n * dy) | n <- [0 .. max (abs (x2 - x1)) (abs (y2 - y1))]]

sign :: Int -> Int
sign n
  | n > 0 = 1
  | n < 0 = -1
  | otherwise = 0
