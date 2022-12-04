module Problems.Y2022.D04.Solution where

import Import

import qualified Solution
import Parsers (lines)

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

type Range = (Int, Int)
type Input = [(Range, Range)]

parse :: Parser Input
parse = lines $ (,) <$> range <* "," <*> range
  where
    range = (,) <$> decimal <* "-" <*> decimal

part1 :: Input -> Int
part1 = length . filter (\(a, b) -> a `contains` b || b `contains` a)

part2 :: Input -> Int
part2 = length . filter (uncurry overlaps)

contains :: Range -> Range -> Bool
contains (a1, b1) (a2, b2) = a1 <= a2 && b1 >= b2

overlaps :: Range -> Range -> Bool
overlaps (a1, b1) (a2, b2) = a1 <= b2 && a2 <= b1
