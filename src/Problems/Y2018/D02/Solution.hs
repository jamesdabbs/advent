module Problems.Y2018.D02.Solution where

import Import hiding (takeWhile)

import Data.Attoparsec.Text (takeWhile)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Solution

solution :: Solution' Input Int String
solution = Solution.basic' parse part1 (fromJust . part2)

type Input = [String]
type Mask = String

parse :: Parser Input
parse = (Text.unpack <$> takeWhile (/= '\n')) `sepBy` "\n"

part1 :: Input -> Int
part1 = uncurry (*) . foldl tally (0, 0)

tally :: (Int, Int) -> String -> (Int, Int)
tally (twos, threes) boxId = (twos + hasCount 2, threes + hasCount 3)
  where
    hasCount n = if any (\(_,v) -> v == n) $ Map.toList $ countBy identity boxId then 1 else 0

part2 :: Input -> Maybe String
part2 = go mempty [] ""
  where
    go :: Map Mask String -> [Mask] -> String -> [String] -> Maybe String
    go lookup (m : ms) id ids = case Map.lookup m lookup of
      Just found -> Just $ lettersInCommon found id
      _ -> go (Map.insert m id lookup) ms id ids
    go lookup [] _ (id : ids) = go lookup (masks id) id ids
    go _ _ _ _ = Nothing

masks :: String -> [Mask]
masks s = map (insertAt s '.') [0 .. length s - 1]

lettersInCommon :: String -> String -> String
lettersInCommon a b = zip a b
  & filter (uncurry (==))
  & map fst
