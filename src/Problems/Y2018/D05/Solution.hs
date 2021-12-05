module Problems.Y2018.D05.Solution where

import Import

import qualified Data.Text as Text
import qualified Solution

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

type Input = String
type Output = Int

parse :: Parser String
parse = Text.unpack <$> takeText

-- 10978
part1 :: String -> Int
part1 = length . react

-- 4840
part2 :: String -> Int
part2 input = minimum [ length $ react $ remove c input | c <- ['A' .. 'Z'] ]

react :: String -> String
react = reverse . go []
  where
    go (a : pre) (b : post)
      | cancels a b = go pre post
      | otherwise = go (b : a : pre) post
    go [] (a : post) = go [a] post
    go pre [] = pre

    cancels a b = a /= b && toUpper a == toUpper b

remove :: Char -> String -> String
remove c = filter (\x -> toUpper x /= toUpper c)