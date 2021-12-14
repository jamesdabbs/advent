module Problems.Y2021.D01.Solution where

import Import
import Parsers (lines)
import qualified Solution

solution :: Solution [Int] Int
solution = Solution.basic parse part1 part2

parse :: Parser [Int]
parse = lines decimal

-- 1602
part1 :: [Int] -> Int
part1 = increases

-- 1633
part2 :: [Int] -> Int
part2 = increases . map sum3 . triples

increases :: Ord a => [a] -> Int
increases = count (uncurry (<)) . pairs

triples :: [a] -> [(a, a, a)]
triples (w : x : y : zs) = (w, x, y) : triples (x : y : zs)
triples _ = []

sum3 :: Num n => (n, n, n) -> n
sum3 (x, y, z) = x + y + z
