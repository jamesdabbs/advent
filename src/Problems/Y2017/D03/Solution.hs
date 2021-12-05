module Problems.Y2017.D03.Solution where

import Import

import Data.Direction (Direction(..))
import qualified Data.Map as Map
import qualified Data.Point as Point
import qualified Solution

solution :: Solution Int Int
solution = Solution.basic' decimal part1 part2

-- 438
part1 :: Int -> Int
part1 input = manhattan $ foldl Point.move (0, 0) $ take input spiral

-- 266330
part2 :: Int -> Int
part2 input = go (Map.singleton (0, 0) 1) (0, 0) spiral
  where
    go scores p (d : ds) =
      let p' = Point.move p d
          score = compute scores p'
      in if score > input
           then score
           else go (Map.insert p' score scores) p' $ traceShow (p', score) ds
    go _ _ _ = 0

    compute scores = sum . catMaybes . map (flip Map.lookup scores) . Point.neighbors

spiral :: [Direction]
spiral = expand $ zip steps $ cycle [E, N, W, S]
  where
    steps = replicate 2 =<< [1 ..]

    expand ((n, a) : as) = replicate n a <> expand as
    expand _ = []
