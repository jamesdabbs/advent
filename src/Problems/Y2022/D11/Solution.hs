module Problems.Y2022.D11.Solution where

import Import hiding (State, evaluate, round)

import qualified Data.Map as Map
import qualified Solution

data Operator  = Add | Mult deriving (Show, Eq)
data Value     = Current | Constant Int deriving (Show, Eq)
data Operation = Operation Operator Value deriving (Show, Eq)

data Monkey = Monkey
  { number    :: Int
  , operation :: Operation
  , test      :: Int
  , ifTrue    :: Int
  , ifFalse   :: Int
  , items     :: [Int]
  , processed :: Int
  } deriving (Show, Eq)

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

type Input = [Monkey]
parse :: Parser Input
parse = monkey `sepBy` "\n"
  where
    monkey = do
      number    <- "Monkey " *> decimal <* ":\n"
      items     <- "  Starting items: " *> decimal `sepBy` ", " <* "\n"
      operation <- "  Operation: new = old " *> operationP <* "\n"
      test      <- "  Test: divisible by " *> decimal <* "\n"
      ifTrue    <- "    If true: throw to monkey " *> decimal <* "\n"
      ifFalse   <- "    If false: throw to monkey " *> decimal <* "\n"
      let processed = 0
      return Monkey{..}

    operationP = Operation
      <$> choice ["+" $> Add, "*" $> Mult]
      <* " "
      <*> choice ["old" $> Current, Constant <$> decimal]

type State = Map Int Monkey

-- 119715
part1 :: Input -> Int
part1 = score . applyN 20 (round f) . prepare
  where
    f n = n `div` 3

-- 18085004878
part2 :: Input -> Int
part2 input = score $ applyN 10000 (round f) $ prepare input
  where
    r = product $ map test input
    f a = a `mod` r

prepare :: Input -> State
prepare = Map.fromList . map (\m -> (number m, m))

turn :: (Int -> Int) -> State -> Int -> State
turn f s i = case Map.lookup i s of
  Nothing -> s
  Just monkey@Monkey{..} -> foldr throw
    (Map.insert i (monkey { items = [], processed = processed + length items }) s)
    (map (process monkey) (reverse items))

  where
    throw (target, value) = flip Map.adjust target $ \monkey ->
      monkey { items = items monkey <> [value] }

    process Monkey{..} v =
      let next   = f (operate operation v)
          target = if next `mod` test == 0 then ifTrue else ifFalse
      in (target, next)

operate :: Operation -> Int -> Int
operate (Operation Add  value) n = n + evaluate value n
operate (Operation Mult value) n = n * evaluate value n

evaluate :: Value -> Int -> Int
evaluate (Constant n) _  = n
evaluate  Current     n  = n

round :: (Int -> Int) -> State -> State
round f s = foldl (turn f) s $ sort $ Map.keys s

score :: State -> Int
score = Map.elems >>> map processed >>> sort >>> reverse >>> take 2 >>> product
