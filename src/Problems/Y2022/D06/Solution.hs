module Problems.Y2022.D06.Solution where

import Import

import qualified Data.Text as Text
import Data.Attoparsec.Text (takeTill)
import qualified Solution

solution :: Solution String Int
solution = Solution.basic parse part1 part2

parse :: Parser String
parse = Text.unpack <$> takeTill (== '\n')

-- 1531
part1 :: String -> Int
part1 = marker 4

-- 2518
part2 :: String -> Int
part2 = marker 14

marker :: Int -> String -> Int
marker k = go 0
  where
    go n l
      | unique (take k l) = n + k
      | otherwise         = go (n + 1) (drop 1 l)

unique :: Ord a => [a] -> Bool
unique l = length (ordNub l) == length l
