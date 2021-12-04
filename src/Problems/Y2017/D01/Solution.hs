module Problems.Y2017.D01.Solution (solution) where

import Import
import Data.Attoparsec.Text (digit, many')

solution :: Solution [Int] Int
solution = Solution parse $ pure . (part1 &&& part2)

parse :: Parser [Int]
parse = many' $ fmap digitToInt digit

part1 :: [Int] -> Int
part1 = captcha 1

part2 :: [Int] -> Int
part2 values = captcha (length values `div` 2) values

captcha :: Int -> [Int] -> Int
captcha offset values =
  zip values (drop offset $ cycle values)
  & filter (uncurry (==))
  & map fst
  & sum
