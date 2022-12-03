module Problems.Y2022.D01.Solution where

import Import

import Parsers (lines)
import qualified Solution

solution :: Solution [[Int]] Int
solution = Solution.basic parse part1 part2

parse :: Parser [[Int]]
parse = lines $ lines decimal

-- 68787
part1 :: [[Int]] -> Int
part1 = map sum >>> maximum

-- 198041
part2 :: [[Int]] -> Int
part2 = map sum >>> sort >>> reverse >>> take 3 >>> sum
