module Problems.Y2021.D03.Solution (solution) where

import Import
import Data.Attoparsec.Text (digit, many')
import Data.List (partition)
import qualified Data.Matrix as Matrix
import qualified Data.Vector as Vector

solution :: Solution (Matrix Bool) Int
solution = Solution parse $ pure . (part1 &&& part2)

parse :: Parser (Matrix Bool)
parse = fmap Matrix.fromLists . lines . many' $
  "0" $> False <|>
  "1" $> True

-- 281 * 3814 = 1071734
part1 :: Matrix Bool -> Int
part1 input = uncurry (*) $ go 0 (0, 0)
  where
    max = Matrix.ncols input

    go n (gamma, epsilon) = case Matrix.safeGetCol (max - n) input of
      Just col -> go (n + 1) $
        if moreTrues col
          then (gamma + 2 ^ n, epsilon)
          else (gamma, epsilon + 2 ^ n)
      _ -> (gamma, epsilon)

moreTrues :: Vector Bool -> Bool
moreTrues input =
  let (ones, zeros) = Vector.partition identity input
   in Vector.length ones >= Vector.length zeros

-- 1679 * 3648 = 6124992
part2 :: Matrix Bool -> Int
part2 = rows
  >>> (go (>=) 0 &&& go (<) 0)
  >>> uncurry (*)
  where
    go :: (Int -> Int -> Bool) -> Int -> [Vector Bool] -> Int
    go cmp i vs =
      let (ones, zeros) = partition (\v -> v Vector.! i) vs
          next = if length ones `cmp` length zeros then ones else zeros
      in case next of
           [vector] -> toInt vector
           [] -> -1
           rest -> go cmp (i + 1) rest

rows :: Matrix a -> [Vector a]
rows m = map (flip Matrix.getRow m) [1 .. Matrix.nrows m]

toInt :: Vector Bool -> Int
toInt v = Vector.foldl f 0 $ Vector.indexed $ Vector.reverse v
  where
    f acc (i, True) = acc + 2 ^ i
    f acc _ = acc
