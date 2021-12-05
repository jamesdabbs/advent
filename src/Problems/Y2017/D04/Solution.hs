module Problems.Y2017.D04.Solution where

import Import

import qualified Data.Attoparsec.Text as P
import qualified Data.Text as Text
import qualified Solution

solution :: Solution Input Output
solution = Solution.basic parse part1 part2

type Input = [[Text]]
type Output = Int

parse :: Parser Input
parse = P.takeWhile1 (\c -> c /= ' ' && c /= '\n') `sepBy1` " " `sepBy1` "\n"

part1 :: Input -> Output
part1 = count valid

part2 :: Input -> Output
part2 = count valid'

valid :: [Text] -> Bool
valid = firstRepeated >>> (== Nothing)

valid' :: [Text] -> Bool
valid' = map (sort . Text.unpack) >>> firstRepeated >>> (== Nothing)
