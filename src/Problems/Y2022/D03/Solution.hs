module Problems.Y2022.D03.Solution where

import Import

import Data.Attoparsec.Text (takeTill)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Solution
import Parsers (lines)

solution :: Solution [Text] Int
solution = Solution.basic parse part1 part2

parse :: Parser [Text]
parse = lines $ takeTill (== '\n')

-- 8202
part1 :: [Text] -> Int
part1 = map (score . head . intersect . pack) >>> sum
  where
    pack a =
      let (l, r)  = Text.splitAt (Text.length a `div` 2) a
       in map chars [l ,r]

-- 2864
part2 :: [Text] -> Int
part2 = eachCons 3 >>>
  map (score . head . intersect . map chars) >>>
  sum

chars :: Text -> Set Char
chars = Set.fromList . Text.unpack

eachCons :: Int -> [a] -> [[a]]
eachCons n l = case splitAt n l of
  ([], []) -> []
  (a,  []) -> [a]
  (a,   b) -> a : eachCons n b

intersect :: Ord a => [Set a] -> Set a
intersect (a:as) = foldr Set.intersection a as
intersect _ = Set.empty

score :: Maybe Char -> Int
score (Just c) = Map.findWithDefault 0 c scores
score _ = 0

scores :: Map Char Int
scores = Map.fromList $ zip (['a' .. 'z'] <> ['A' .. 'Z']) [1..]
