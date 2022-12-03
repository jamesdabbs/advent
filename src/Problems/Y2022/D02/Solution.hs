module Problems.Y2022.D02.Solution where

import Import

import qualified Solution
import Parsers (lines)

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

type Input = [(Move, Instruction)]

data Instruction = X    | Y     | Z        deriving (Show, Eq)
data Move        = Rock | Paper | Scissors deriving (Show, Eq)
data Result      = Win  | Lose  | Draw     deriving (Show, Eq)

parse :: Parser Input
parse = lines $ do
  a <- choice ["A" $> Rock, "B" $> Paper, "C" $> Scissors]
  " "
  b <- choice ["X" $> X, "Y" $> Y, "Z" $> Z]
  return (a, b)

-- 10310
part1 :: Input -> Int
part1 = sum . map score . map (identity *** interpret)
  where
    interpret X = Rock
    interpret Y = Paper
    interpret Z = Scissors

-- 14859
part2 :: Input -> Int
part2 = sum . map score . map f
  where
    interpret X = Lose
    interpret Y = Draw
    interpret Z = Win

    f (move, result) = (move, unplay move $ interpret result)

winner :: Move -> Move
winner Rock     = Paper
winner Paper    = Scissors
winner Scissors = Rock

play :: Move -> Move -> Result
play a b
  | a == b        = Draw
  | a == winner b = Win
  | otherwise     = Lose

unplay :: Move -> Result -> Move
unplay a Draw = a
unplay a Win  = winner a
unplay a _    = winner (winner a)

score :: (Move, Move) -> Int
score (a, b) = move b + result (play b a)
  where
    move Rock     = 1
    move Paper    = 2
    move Scissors = 3

    result Lose = 0
    result Draw = 3
    result Win  = 6