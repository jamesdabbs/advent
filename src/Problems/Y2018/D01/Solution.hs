module Problems.Y2018.D01.Solution where

import Import
import Parsers (lines)

import qualified Solution

solution :: Solution' [Int] Int (Maybe Int)
solution = Solution.basic' parse sum part2

parse :: Parser [Int]
parse = lines $ signed decimal

part2 :: [Int] -> Maybe Int
part2 = cycle >>> scanl (+) 0 >>> firstRepeated
