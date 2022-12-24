module Problems.Y2022.D14.Solution where

import qualified Data.Set as Set
import Import
import Parsers (lines)
import qualified Solution

solution :: Solution Grid Int
solution = Solution.basic parse part1 part2

type Grid = Set Point

parse :: Parser Grid
parse = fmap plot . lines $ run `sepBy` " -> "
  where
    run = (,) <$> decimal <* "," <*> decimal

    plot = foldr Set.union Set.empty . concatMap segments

    segments (a : b : cs) = fill a b : segments (b : cs)
    segments _ = []

    fill (x1, y1) (x2, y2) = Set.fromList [(x, y) | x <- range x1 x2, y <- range y1 y2]

    range a b = [min a b .. max a b]

-- 1001
part1 :: Grid -> Int
part1 grid = flowUntil (\(_, y) -> y == bound - 1) grid
  where
    bound = maximum (Set.map snd grid) + 2

-- 27976
part2 :: Grid -> Int
part2 = (+ 1) . flowUntil (== source)

source :: Point
source = (500, 0)

flowUntil :: (Point -> Bool) -> Grid -> Int
flowUntil cond grid = go 0 Set.empty
  where
    bound = maximum (Set.map snd grid) + 2

    go n sand =
      let p = settle bound (grid <> sand) source
       in if cond p then n else go (n + 1) (p `Set.insert` sand)

settle :: Int -> Grid -> Point -> Point
settle bound taken p =
  let p' = fall bound taken p
   in if p == p' then p else settle bound taken p'

fall :: Int -> Grid -> Point -> Point
fall bound board (x, y) =
  [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
    & filter (\p -> snd p < bound)
    & find (not . flip Set.member board)
    & fromMaybe (x, y)
