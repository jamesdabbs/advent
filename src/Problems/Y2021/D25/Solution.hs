module Problems.Y2021.D25.Solution where

import Import hiding (round)

import qualified Data.Map as Map
import Parsers (pointGrid)
import qualified Solution

solution :: Solution' Input Output ()
solution = Solution.basic parse part1 part2

data Space = East | South | Empty
  deriving (Show, Eq, Ord)

type Input = Map Point Space
type Output = Int
type Output' = Output

parse :: Parser Input
parse = pointGrid $ choice
  [ ">" $> East
  , "v" $> South
  , "." $> Empty
  ]

part1 :: Input -> Int
part1 input = go 1 input
  where
    go round positions =
      let next = evolve positions
      in if next == positions then round else go (round + 1) next

    evolve = moveHerd East east >>> moveHerd South south

    moveHerd herd next positions = Map.foldlWithKey (move herd next positions) positions positions

    move herd next previous current p value
      | value == herd =
          case Map.lookup (next p) previous of
            Just Empty -> Map.insert (next p) value $ Map.insert p Empty current
            _ -> current
      | otherwise = current

    east (x, y)
      | x == maxX = (0,     y)
      | otherwise = (x + 1, y)

    south (x, y)
      | y == maxY = (x,     0)
      | otherwise = (x, y + 1)

    maxX = maximum $ map fst $ Map.keys input
    maxY = maximum $ map snd $ Map.keys input

part2 :: Input -> ()
part2 _ = ()
