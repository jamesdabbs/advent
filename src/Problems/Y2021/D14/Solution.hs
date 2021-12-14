module Problems.Y2021.D14.Solution where

import Import

import qualified Data.Map as Map
import qualified Solution

solution :: Solution Input Output
solution = Solution.basic parse part1 part2

data Input = Input
  { template :: String
  , rules :: Rules
  } deriving (Show)
type Output = Int

type Rules = Map (Char, Char) Char
type Counts = Map Char Int

parse :: Parser Input
parse = Input
  <$> many' (notChar '\n')
  <* "\n\n"
  <*> fmap Map.fromList (rule `sepBy` "\n")
  where
    rule = (,)
      <$> ((,) <$> anyChar <*> anyChar)
      <* " -> "
      <*> anyChar

part1 :: Input -> Output
part1 = score 10

part2 :: Input -> Output
part2 = score 40

score :: Int -> Input -> Int
score rounds Input{..} = spread $ combineCounts
  $ countBy identity template : lookupExpansions template
  where
    spread vs = maximum (Map.elems vs) - minimum (Map.elems vs)

    lookupExpansions = map (\pair -> Map.findWithDefault mempty pair expansions) . pairs

    expansions = generateExpansions rules rounds

generateExpansions :: Rules -> Int -> Map (Char, Char) Counts
generateExpansions rules rounds = applyN rounds generate mempty
  where
    generate results = Map.mapWithKey (expand results) rules

    expand acc (a, b) _ = case Map.lookup (a, b) rules of
      Just c -> combineCounts
        [ Map.findWithDefault mempty (a, c) acc
        , Map.singleton c 1
        , Map.findWithDefault mempty (c, b) acc
        ]
      Nothing -> mempty

combineCounts :: (Ord k, Num n) => [Map k n] -> Map k n
combineCounts = foldl (Map.unionWith (+)) mempty
