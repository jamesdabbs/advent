module Problems.Y2021.D11.Solution where

import Import

import Data.List (findIndex)
import qualified Data.Map as Map
import Data.Point (neighbors)
import qualified Data.Set as Set
import Parsers (pointGrid)
import qualified Solution

solution :: Solution Input Output
solution = Solution.basic parse part1 part2

type Input = Map Point Int
type Output = Int

parse :: Parser Input
parse = pointGrid $ fmap digitToInt digit

part1 :: Input -> Output
part1 = generations
  >>> take 101
  >>> map flashed
  >>> sum

part2 :: Input -> Output
part2 = generations
  >>> findIndex (liftM2 (==) flashed Map.size)
  >>> fromJust

generations :: Input -> [Input]
generations = iterate tick

tick :: Input -> Input
tick = go mempty . Map.map (+1)
  where
    go acc octopi =
      let flashes = Map.keysSet $ Map.filter (> 9) octopi
          new = flashes Set.\\ acc
      in if Set.null new
           then Set.fold (flip Map.insert 0) octopi flashes
           else go flashes $ Set.foldl flash octopi new

    flash octopi = foldl excite octopi . neighbors

    excite m a = Map.alter (fmap (+1)) a m

flashed :: Input -> Int
flashed = count (== 0) . Map.elems
