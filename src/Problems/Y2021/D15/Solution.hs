module Problems.Y2021.D15.Solution where

import Import

import Data.Map ((!))
import qualified Data.Map.Strict as Map
import Parsers (pointGrid)
import qualified Solution

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

type Input = Map Point Int

parse :: Parser Input
parse = pointGrid $ fmap digitToInt digit

part1 :: Input -> Int
part1 input = foldl' f (Map.singleton point 0) points ! (0, 0)
  where
    f acc (x, y) =
      let paths = catMaybes $ map
                    (\p -> liftA2 (+) (Map.lookup p input) (Map.lookup p acc))
                    [(x + 1, y), (x, y + 1)]
      in Map.insert (x, y) (minimum paths) acc

    point : points = reverse $ sortOn (uncurry (+)) $ Map.keys input

part2 :: Input -> Int
part2 = part1 . expand 5
  where
    expand n acc = Map.fromList $ do
      let (maxX, maxY) = foldl (\(a, b) (c, d) -> (max a c, max b d)) (0, 0) $ Map.keys acc
      ((x, y), v) <- Map.toList acc
      i <- [0 .. n - 1]
      j <- [0 .. n - 1]
      return
        ( ( x + i * (maxX + 1)
          , y + j * (maxY + 1)
          )
        , wrap (v + i + j)
        )

    wrap n
      | n > 9 = n - 9
      | otherwise = n
