module Problems.Y2022.D18.Solution where

import Import hiding (Point)

import qualified Data.Set as Set
import Parsers (lines)
import qualified Solution

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

type Point = (Int, Int, Int)
type Input = Set Point

parse :: Parser Input
parse = fmap Set.fromList . lines $
  (,,) <$> decimal <* "," <*> decimal <* "," <*> decimal

-- 3576
part1 :: Input -> Int
part1 input = concatMap neighbors input & filter (\x -> not $ x `Set.member` input) & length

neighbors :: Point -> [Point]
neighbors (x,y,z) =
  [ (x + 1, y, z)
  , (x - 1, y, z)
  , (x, y + 1, z)
  , (x, y - 1, z)
  , (x, y, z + 1)
  , (x, y, z - 1)
  ]

-- 2066
part2 :: Input -> Int
part2 droplet = Set.size $ visit [(xmin, ymin, zmin)] Set.empty Set.empty
  where
    (xmin, xmax) = bounds [ x | (x, _, _) <- Set.toList droplet ]
    (ymin, ymax) = bounds [ y | (_, y, _) <- Set.toList droplet ]
    (zmin, zmax) = bounds [ z | (_, _, z) <- Set.toList droplet ]

    bounds rs = (minimum rs - 1, maximum rs + 1)

    inBounds (x, y, z) = xmin <= x && x <= xmax && ymin <= y && y <= ymax && zmin <= z && z <= zmax

    visit (p : ps) visited faces =
      let (lava, air) = partition (`Set.member` droplet) $ filter inBounds $ neighbors p
          newFaces    = Set.fromList [ (p, l) | l <- lava]
          newAir      = filter (\x -> not $ x `Set.member` visited) air
      in visit (newAir <> ps) (Set.insert p visited) (Set.union faces newFaces)
    visit [] _ acc = acc
