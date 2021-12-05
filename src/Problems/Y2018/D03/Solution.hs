module Problems.Y2018.D03.Solution where

import Import

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Solution

solution :: Solution Input Int
solution = Solution.basic parse part1 (fromJust . part2)

data Claim = Claim
  { id     :: Int
  , left   :: Int
  , top    :: Int
  , width  :: Int
  , height :: Int
  } deriving (Show, Eq, Ord)

type Grid = Map Point (Set Claim)

type Input = [Claim]

parse :: Parser Input
parse = claim `sepBy` "\n"
  where
    claim = do
      "#"
      id <- decimal
      " @ "
      left <- decimal
      ","
      top <- decimal
      ": "
      width <- decimal
      "x"
      height <- decimal
      return Claim{..}

part1 :: Input -> Int
part1 claims =
  let grid = foldr' apply mempty claims
  in length $ filter (\s -> Set.size s > 1) $ Map.elems grid

part2 :: Input -> Maybe Int
part2 claims =
  let grid = foldr' apply mempty claims
  in id <$> find (unconflicted grid) claims

apply :: Claim -> Grid -> Grid
apply claim grid = foldr' add grid $ extent claim
  where
    add p = Map.insertWith Set.union p (Set.singleton claim)

extent :: Claim -> [Point]
extent Claim{..} = (,)
  <$> [left .. left + width - 1]
  <*> [top  .. top + height - 1]

unconflicted :: Grid -> Claim -> Bool
unconflicted grid c = flip all (extent c) $ \point ->
  Map.lookup point grid == (Just $ Set.singleton c)
