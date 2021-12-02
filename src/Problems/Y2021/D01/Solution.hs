module Problems.Y2021.D01.Solution
  ( solution
  ) where

import Protolude
import Data.Attoparsec.Text (Parser, decimal, sepBy)
import Solution (SolutionM(Solution), Solution)
import Utils (count)

solution :: Solution [Int] Int
solution = Solution parse (pure . run)

parse :: Parser [Int]
parse = decimal `sepBy` "\n"

run :: [Int] -> (Int, Int)
run values =
  -- (1602,1633)
  ( increases values
  , increases $ map sum3 $ triples values
  )

increases :: Ord a => [a] -> Int
increases = count (uncurry (<)) . pairs

pairs :: [a] -> [(a, a)]
pairs (x : y : zs) = (x, y) : pairs (y : zs)
pairs _ = []

triples :: [a] -> [(a, a, a)]
triples (w : x : y : zs) = (w, x, y) : triples (x : y : zs)
triples _ = []

sum3 :: Num n => (n, n, n) -> n
sum3 (x, y, z) = x + y + z
