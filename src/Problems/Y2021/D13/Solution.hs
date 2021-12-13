module Problems.Y2021.D13.Solution where

import Import

import Data.Attoparsec.Text (char)
import qualified Data.Set as Set
import qualified Solution

solution :: Solution' Input Int String
solution = Solution.basic parse part1 part2

type Input = (Set Point, [Line])

data Axis = X | Y deriving (Show, Eq, Ord, Enum, Bounded)
data Line = Line Axis Int deriving (Show)

parse :: Parser Input
parse = (,)
  <$> fmap Set.fromList (point `sepBy` "\n")
  <* "\n\n"
  <*> ("fold along " *> line) `sepBy` "\n"
  where
    point = (,) <$> decimal <* "," <*> decimal
    line = Line
      <$> (char 'x' $> X <|> char 'y' $> Y)
      <* "="
      <*> decimal

part1 :: Input -> Int
part1 (points, lines) = Set.size $ mirror (fromJust $ head lines) points

part2 :: Input -> String
part2 (points, lines) = "\n" <> render (foldl (flip mirror) points lines)

mirror :: Line -> Set Point -> Set Point
mirror (Line X n) = Set.map $ \(x, y) -> (n - abs (x - n), y)
mirror (Line Y n) = Set.map $ \(x, y) -> (x, n - abs (y - n))

render :: Set Point -> String
render pts = intercalate "\n" $
  flip map [ymin .. ymax] $ \y ->
    flip map [xmin .. xmax] $ \x ->
      if Set.member (x,y) pts then '■' else ' '
  where
    (xmin, ymin, xmax, ymax) = extent pts

    extent = foldl
      (\(a, b, c, d) (x, y) -> (min x a, min y b, max x c, max y d))
      (0, 0, 0, 0)
