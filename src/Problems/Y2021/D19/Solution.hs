module Problems.Y2021.D19.Solution where

import Import hiding (Point)

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Solution

solution :: Solution' Input Output Output'
solution = Solution.basic parse part1 part2

type Input = [Region]
type Output = Int
type Output' = ()

type Point = (Int, Int, Int)
data Region = Region
  { number :: [Int]
  , points :: Set Point
  , distances :: Set Int
  } deriving (Show, Eq)

parse :: Parser Input
parse = scanner `sepBy` "\n\n"
  where
    scanner = makeRegion
      <$> ("--- scanner " *> decimal <* " ---\n")
      <*> point `sepBy` "\n"

    point = (,,) <$> signed decimal <* "," <*> signed decimal <* "," <*> signed decimal

part1 :: Input -> Output
part1 input =
  -- compute internal distances for each scanner
  -- pick pair of scanners with maximal distance overlap
    -- join into region
  -- repeatedly check for unused scanner with maximal distance overlap with region
    -- join
  let
    (a, b) = fromJust $ head $ reverse $ sortOn (uncurry similarity) $ [(x,y) | x <- input, y <- input, x /= y]
    c = combine a b
  in
    traceShow c $ length input

part2 :: Input -> Output'
part2 input = ()

range :: Int
range = 1000

makeRegion :: Int -> [Point] -> Region
makeRegion number points = Region
  { number = [number]
  , points = Set.fromList points
  , distances = Set.fromList [ dist a b | a <- points, b <- points, a /= b ]
  }

dist :: Point -> Point -> Int
dist (x1, y1, z1) (x2, y2, z2) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

similarity :: Region -> Region -> Int
similarity a b = Set.size $ Set.intersection (distances a) (distances b)

combine :: Region -> Region -> Region
combine a b = a
