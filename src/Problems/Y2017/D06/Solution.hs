module Problems.Y2017.D06.Solution where

import Import

import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Solution

solution :: Solution (Vector Int) Int
solution = Solution.basic parse part1 part2

parse :: Parser (Vector Int)
parse = Vector.fromList <$> decimal `sepBy` "\t"

part1 :: Vector Int -> Int
part1 input =
  let Just (_, _, n) = findRepeat $ iterate step input
  in n

part2 :: Vector Int -> Int
part2 input =
  let Just (_, i, n) = findRepeat $ iterate step input
  in n - i

step :: Vector Int -> Vector Int
step st =
  let i = Vector.maxIndex st
      m = st Vector.! i
  in redistribute m ((i + 1) `mod` Vector.length st) $ st Vector.// [(i, 0)]

redistribute :: Int -> Int -> Vector Int -> Vector Int
redistribute 0 _ v = v
redistribute amt ind v = redistribute (amt - 1) ((ind + 1) `mod` len)
  $ v Vector.// [(ind, (v Vector.! ind) + 1)]
  where
    len = Vector.length v

findRepeat :: Ord a => [a] -> Maybe (a, Int, Int)
findRepeat ls = go ls 0 mempty
  where
    go (x : xs) n seen = case Map.lookup x seen of
      Just i -> Just (x, i, n)
      _ -> go xs (n + 1) $ Map.insert x n seen
    go [] _ _ = Nothing