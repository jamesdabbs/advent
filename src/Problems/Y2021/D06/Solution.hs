module Problems.Y2021.D06.Solution where

import Import hiding (Vector)

import qualified Data.Map as Map
import Data.Vector.Unboxed as Vector (Vector, (!), generate, sum)
import qualified Solution

solution :: Solution Input Output
solution = Solution.basic parse part1 part2

type Input = Vector Int
type Output = Int

parse :: Parser Input
parse = pack <$> decimal `sepBy` ","

part1 :: Input -> Output
part1 = fishAfter 80

part2 :: Input -> Output
part2 = fishAfter 256

fishAfter :: Int -> Input -> Int
fishAfter days = applyN days step >>> Vector.sum

pack :: [Int] -> Vector Int
pack rawFish = Vector.generate 9 $ \age -> Map.findWithDefault 0 age counts
  where
    counts = countBy identity rawFish

step :: Vector Int -> Vector Int
step v = Vector.generate 9 $ \case
  8 -> v ! 0
  6 -> v ! 7 + v ! 0
  n -> v ! (n + 1)
