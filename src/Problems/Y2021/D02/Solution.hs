module Problems.Y2021.D02.Solution (solution) where

import Import hiding (Down)

data Direction
  = Forward
  | Up
  | Down
  deriving (Show, Eq)

type Instruction = (Direction, Int)

solution :: Solution [Instruction] Int
solution = Solution parse (pure . run)

parse :: Parser [Instruction]
parse = lines $
  (,) <$> direction <* " " <*> decimal
  where
    direction = choice
      [ "forward" *> pure Forward
      , "up" *> pure Up
      , "down" *> pure Down
      ]

run :: [Instruction] -> (Int, Int)
run instructions =
  -- (1524750,1592426537)
  ( uncurry (*) $ foldl follow (0, 0) instructions
  , uncurry (*) . fst2 $ foldl follow' (0, 0, 0) instructions
  )

follow :: (Int, Int) -> Instruction -> (Int, Int)
follow (x, y) (Forward, n) = (x + n, y)
follow (x, y) (Down, n) = (x, y + n)
follow (x, y) (Up, n) = (x, y - n)

follow' :: (Int, Int, Int) -> Instruction -> (Int, Int, Int)
follow' (x, y, aim) (Forward, n) = (x + n, y + n * aim, aim)
follow' (x, y, aim) (Down, n) = (x, y, aim + n)
follow' (x, y, aim) (Up, n) = (x, y, aim - n)

fst2 :: (x, y, z) -> (x, y)
fst2 (x, y, z) = (x, y)
