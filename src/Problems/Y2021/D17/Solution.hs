module Problems.Y2021.D17.Solution where

import Import

import qualified Solution

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

type Input = (Range, Range)
type Range = (Int, Int)
type Probe = (Point, Point)

parse :: Parser Input
parse = "target area: x=" >>
  (,)
    <$> range
    <* ", y="
    <*> range
  where
    range = (,) <$> signed decimal <* ".." <*> signed decimal

part1 :: Input -> Int
part1 = arcs >>> map snd >>> maximum

part2 :: Input -> Int
part2 = arcs >>> length

arcs :: Input -> [(Point, Int)]
arcs input@((x1, x2), (y1, _)) = do
  x <- [minVelocity x1 .. x2]
  y <- [y1 .. abs y1]
  case arc input 0 $ tick ((0, 0), (x, y)) of
    Just height -> return ((x, y), height)
    Nothing -> []

arc :: Input -> Int -> Probe -> Maybe Int
arc input@((x1, x2), (y1, y2)) h ((x, y), (xv, yv))
  | x >= x1 && x <= x2 && y >= y1 && y <= y2 = Just h
  | x >= x2 = Nothing
  | yv < 0 && y < y1 = Nothing
  | otherwise = arc input (max h y) $ tick ((x, y), (xv, yv))

tick :: Probe -> Probe
tick ((x, y), (xv, yv)) = ((x + xv, y + yv), (if xv > 0 then xv - 1 else 0, yv - 1))

minVelocity :: Int -> Int
minVelocity target = ceiling $ (sqrt (1 + 8 * fromIntegral target :: Double) - 1) / 2
