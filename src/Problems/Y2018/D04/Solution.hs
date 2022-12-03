module Problems.Y2018.D04.Solution where

import Import

import qualified Solution

solution :: Solution' Input Output Output'
solution = Solution.basic parse part1 part2

type Input = [Event]
type Output = ()
type Output' = Output

data Event = Event
  { ts :: Timestamp
  , change :: Change
  } deriving (Show, Eq)

data Timestamp = Timestamp
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minute :: Int
  } deriving (Show, Eq, Ord)

data Change
  = Start Int
  | Sleep
  | Wake
  deriving (Show, Eq)

parse :: Parser Input
parse = event `sepBy` "\n"
  where
    event = Event
      <$> timestamp
      <* " "
      <*> action

    timestamp = do
      "["
      year <- decimal
      "-"
      month <- decimal
      "-"
      day <- decimal
      " "
      hour <- decimal
      ":"
      minute <- decimal
      "]"
      return Timestamp{..}

    action = choice
      [ "Guard #" >> (Start <$> decimal <* " begins shift")
      , "falls asleep" $> Sleep
      , "wakes up" $> Wake
      ]

part1 :: Input -> Output
part1 input = ()

part2 :: Input -> Output'
part2 input = part1 input
