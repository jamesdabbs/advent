module Problems.Y2021.D10.Solution where

import Import

import Data.Attoparsec.Text (notChar)
import qualified Solution

solution :: Solution Input Output
solution = Solution.basic parse part1 part2

type Input = [String]
type Output = Int

parse :: Parser Input
parse = many1 (notChar '\n') `sepBy` "\n"

part1 :: Input -> Output
part1 = sum . map (score . process [])
  where
    score (Left ')') = 3
    score (Left ']') = 57
    score (Left '}') = 1197
    score (Left '>') = 25137
    score _ = 0

part2 :: Input -> Output
part2 = middle . catMaybes . map (score 0 . process [])
  where
    middle l = fromJust $ head $ drop (length l `div` 2) $ sort l

    score _ (Left _) = Nothing
    score acc (Right []) = Just acc
    score acc (Right (c:cs)) = score
      ( 5 * acc + case c of
        ')' -> 1
        ']' -> 2
        '}' -> 3
        _ -> 4
      ) (Right cs)

process :: String -> String -> Either Char String
process s [] = Right s
process s ('(' : l) = process (')' : s) l
process s ('[' : l) = process (']' : s) l
process s ('{' : l) = process ('}' : s) l
process s ('<' : l) = process ('>' : s) l
process (s:ss) (l:ls)
  | s == l = process ss ls
  | otherwise = Left l
process _ (c : _) = Left c
