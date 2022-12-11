module Problems.Y2022.D08.Solution where

import qualified Data.Map as Map
import Import
import Parsers (pointGrid)
import qualified Solution

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

type Input = Map Point Int

data Directional a = Directional
  { up :: a,
    right :: a,
    down :: a,
    left :: a
  }

type Views = (Maybe Point, Maybe Point, Maybe Point, Maybe Point)

parse :: Parser Input
parse = pointGrid $ fmap digitToInt digit

-- 1796
part1 :: Input -> Int
part1 g = length $ filter visible $ map (views g) $ Map.keys g

visible :: Directional (Maybe a) -> Bool
visible Directional {..} = any isNothing [up, right, down, left]

views :: Input -> Point -> Directional (Maybe Point)
views grid point =
  Directional
    { right = view grid point (1, 0),
      down = view grid point (0, 1),
      left = view grid point (-1, 0),
      up = view grid point (0, -1)
    }

view :: Input -> Point -> (Int, Int) -> Maybe Point
view grid p (dx, dy) = go p
  where
    next (x, y) = (x + dx, y + dy)

    height = Map.findWithDefault 0 p grid

    go q = case Map.lookup (next q) grid of
      Just v -> if v < height then go (next q) else Just $ next q
      _ -> Nothing

-- 288120
part2 :: Input -> Int
part2 grid = maximum $ map (score grid) $ Map.keys grid

score :: Input -> Point -> Int
score grid point@(x, y) =
  let bs = bounds grid
      vs = views grid point
   in product
        [ maybe (right bs) fst (right vs) - x,
          maybe (down bs) snd (down vs) - y,
          x - maybe (left bs) fst (left vs),
          y - maybe (up bs) snd (up vs)
        ]

bounds :: Map Point a -> Directional Int
bounds grid =
  let keys = Map.keys grid
      xs = map fst keys
      ys = map snd keys
   in Directional {up = minimum ys, right = maximum xs, down = maximum ys, left = minimum xs}
