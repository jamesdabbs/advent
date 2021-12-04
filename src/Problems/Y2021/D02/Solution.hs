module Problems.Y2021.D02.Solution where

import Import
import Parsers (lines)
import qualified Solution

data Direction
  = Forward
  | Up
  | Down
  deriving (Show, Eq)

type Instruction = (Direction, Int)
type Position = (Int, Int)

solution :: Solution [Instruction] Int
solution = Solution.basic parse part1 part2

parse :: Parser [Instruction]
parse = lines $
  (,) <$> direction <* " " <*> decimal
  where
    direction = choice
      [ "forward" $> Forward
      , "up" $> Up
      , "down" $> Down
      ]

-- 1524750
part1 :: [Instruction] -> Int
part1 = foldl follow (0, 0) >>> uncurry (*)

-- 1592426537
part2 :: [Instruction] -> Int
part2 = foldl follow' ((0, 0), 0) >>> fst >>> uncurry (*)

follow :: Position -> Instruction -> Position
follow (x, y) (Forward, n) = (x + n, y)
follow (x, y) (Down, n) = (x, y + n)
follow (x, y) (Up, n) = (x, y - n)

follow' :: (Position, Int) -> Instruction -> (Position, Int)
follow' ((x, y), aim) (Forward, n) = ((x + n, y + n * aim),     aim)
follow' ((x, y), aim) (Down,    n) = ((    x,           y), aim + n)
follow' ((x, y), aim) (Up,      n) = ((    x,           y), aim - n)
