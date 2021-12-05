module Problems.Y2017.D05.Solution where

import Import

import qualified Data.Vector as Vector
import qualified Solution

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

type Input = Vector Int

parse :: Parser Input
parse = Vector.fromList <$> signed decimal `sepBy` "\n"

-- 339351
part1 :: Input -> Int
part1 = escape (+1) 0 0

-- 24315397
part2 :: Input -> Int
part2 = escape (\v -> if v >= 3 then v - 1 else v + 1) 0 0

escape :: (Int -> Int) -> Int -> Int -> Vector Int -> Int
escape update pos i vec = case vec Vector.!? pos of
  Nothing  -> i
  Just val -> escape update (pos + val) (i + 1) $ vec Vector.// [(pos, update val)]