module Problems.Y2021.D07.Solution where

import Import hiding (from, to)

import qualified Data.Map as Map
import qualified Solution

solution :: Solution Crabs Int
solution = Solution.basic parse part1 part2

type Crabs = Map Int Int
type Cost = Int -> Int -> Int

parse :: Parser Crabs
parse = countBy identity <$> decimal `sepBy` ","

part1 :: Crabs -> Int
part1 = solve distance

part2 :: Crabs -> Int
part2 = solve $ \from to -> quadSum $ distance from to

solve :: Cost -> Crabs -> Int
solve cost input = minimum $ map (totalCost input) $ positions input
  where
    totalCost crabs to = sum $ map (pointwiseCost to) $ Map.assocs crabs

    pointwiseCost to (from, population) = population * cost from to

positions :: Map Int a -> [Int]
positions input
  | Map.null input = []
  | otherwise = [fst (Map.findMin input) .. fst (Map.findMax input)]

distance :: Int -> Int -> Int
distance a b = abs $ b - a

quadSum :: Int -> Int
quadSum n = n * (n + 1) `div` 2
