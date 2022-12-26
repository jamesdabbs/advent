module Problems.Y2022.D15.Solution where

import qualified Data.Set as Set
import Import
import Parsers (lines)
import qualified Solution

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

data Ping = Ping {sensor :: Point, beacon :: Point} deriving (Show)

data Range = Range Int Int deriving (Show, Eq)

type Input = [Ping]

parse :: Parser Input
parse =
  lines $
    "Sensor at " >> Ping <$> point <* ": closest beacon is at " <*> point
  where
    point = (,) <$> ("x=" *> signed decimal) <* ", y=" <*> signed decimal

-- 4985193
part1 :: Input -> Int
part1 input =
  let row = 2000000
      coverage = coverageAt row input
      beacons = ordNub [x | (x, y) <- map beacon input, y == row]
      coveredBeacons = filter (`member` coverage) beacons
   in size coverage - length coveredBeacons

-- 11583882601918
part2 :: [Ping] -> Int
part2 input = fromJust $ do
  let rows = map (\y -> (y, coverageAt y input)) [0 .. bound]
  (y, ranges) <- find (not . flip covers (Range 0 bound) . snd) rows
  x <- missing ranges
  return $ bound * x + y
  where
    bound = 4000000

    missing [Range _ a, _] = Just $ a + 1
    missing _ = Nothing

coverageAt :: Int -> [Ping] -> [Range]
coverageAt y = foldl' (\acc -> maybe acc (add acc) . rangeAt y) []

rangeAt :: Int -> Ping -> Maybe Range
rangeAt h (Ping (x, y) b) =
  let d = distance (x, y) b - abs (h - y)
   in if d >= 0 then Just (Range (x - d) (x + d)) else Nothing

expandY :: Int -> [Range] -> Set Point
expandY y = foldr (Set.union . toSet) Set.empty
  where
    toSet (Range a b) = Set.fromList [(x, y) | x <- [a .. b]]

distance :: Point -> Point -> Int
distance (x, y) (a, b) = abs (x - a) + abs (y - b)

add :: [Range] -> Range -> [Range]
add [] r = [r]
add (a@(Range a1 a2) : as) b@(Range b1 b2)
  | b2 + 1 < a1 = b : a : as
  | a2 + 1 < b1 = a : add as b
  | otherwise = add as (Range (min a1 b1) (max a2 b2))

member :: Int -> [Range] -> Bool
member _ [] = False
member x ((Range a b) : rs)
  | a <= x && x <= b = True
  | otherwise = member x rs

covers :: [Range] -> Range -> Bool
covers rs r = add rs r == rs

size :: [Range] -> Int
size = sum . map (\(Range a b) -> b - a + 1)