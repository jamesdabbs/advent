module Problems.Y2022.D09.Solution where

import qualified Data.Set as Set
import Import hiding (Left, Right, State, head)
import Parsers (lines)
import qualified Solution

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

data Direction = Up | Right | Down | Left deriving (Show, Eq)

type Input = [Direction]

data State = State
  { head :: Point,
    tail :: [Point],
    visited :: Set Point
  }
  deriving (Show, Eq)

parse :: Parser Input
parse = concat <$> lines directions
  where
    directions = flip replicate <$> direction <* " " <*> decimal

    direction =
      choice
        [ "U" $> Up,
          "R" $> Right,
          "D" $> Down,
          "L" $> Left
        ]

-- 6236
part1 :: [Direction] -> Int
part1 = Set.size . visited . foldl tick (initial 2)

-- 2449
part2 :: [Direction] -> Int
part2 = Set.size . visited . foldl tick (initial 10)

initial :: Int -> State
initial n =
  State
    { head = (0, 0),
      tail = replicate (n - 1) (0, 0),
      visited = Set.singleton (0, 0)
    }

tick :: State -> Direction -> State
tick State {..} direction =
  let head' = move direction head
      (tail', last) = pull [] head' tail
      visited' = Set.insert last visited
   in State {head = head', tail = tail', visited = visited'}

pull :: [Point] -> Point -> [Point] -> ([Point], Point)
pull acc d (p : ps) =
  let p' = follow d p
   in pull (p' : acc) p' ps
pull acc end _ = (reverse acc, end)

move :: Direction -> Point -> Point
move Up (x, y) = (x, y + 1)
move Right (x, y) = (x + 1, y)
move Down (x, y) = (x, y - 1)
move Left (x, y) = (x - 1, y)

follow :: Point -> Point -> Point
follow (x1, y1) (x2, y2) =
  let dx = x1 - x2
      dy = y1 - y2
   in if abs dx > 1 || abs dy > 1
        then (x2 + sign dx, y2 + sign dy)
        else (x2, y2)

sign :: Int -> Int
sign 0 = 0
sign n = n `div` abs n
