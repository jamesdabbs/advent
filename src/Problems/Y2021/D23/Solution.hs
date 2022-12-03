module Problems.Y2021.D23.Solution where

import Import

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Solution

solution :: Solution' Input Output Output'
solution = Solution.basic parse part1 part2

type Input = ()
type Output = ()
type Output' = Output

parse :: Parser Input
parse = return ()

part1 :: Input -> Output
part1 input = ()

part2 :: Input -> Output'
part2 input = part1 input
