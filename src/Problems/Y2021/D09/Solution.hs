module Problems.Y2021.D09.Solution where

import Import

import qualified Data.Matrix as Matrix
import Data.Matrix ((!))
import qualified Data.Set as Set
import Data.Set ((\\))
import qualified Solution

solution :: Solution Input Output
solution = Solution.basic parse part1 part2

type Input = Matrix Int
type Output = Int

parse :: Parser Input
parse = Matrix.fromLists <$> many1 (fmap digitToInt digit) `sepBy` "\n"

part1 :: Input -> Output
part1 = lowPoints
  >>> map (snd >>> (+1))
  >>> sum

part2 :: Input -> Output
part2 input = lowPoints input
  & map (Set.size . fill input . fst)
  & sort
  & reverse
  & take 3
  & product

fill :: Input -> Point -> Set Point
fill input = expand mempty . Set.singleton
  where
    terminal p = maybe True (== 9) $ uncurry Matrix.safeGet p input

    expand interior boundary = case Set.minView boundary of
      Just (p, boundary') ->
        let
          expansion = neighbors p
              & Set.fromList
              & \s -> s \\ interior
              & Set.filter (not . terminal)
        in expand (Set.insert p interior) (boundary' <> expansion)
      _ -> interior

lowPoints :: Input -> [(Point, Int)]
lowPoints input = do
  x <- [1 .. Matrix.nrows input]
  y <- [1 .. Matrix.ncols input]
  let
    p = (x, y)
    n = input ! (x, y)
  guard $ all (n <) $ neighborValues input p
  return (p, n)

neighborValues :: Input -> Point -> [Int]
neighborValues input = catMaybes . map (\p -> uncurry Matrix.safeGet p input) . neighbors

neighbors :: Point -> [Point]
neighbors (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
