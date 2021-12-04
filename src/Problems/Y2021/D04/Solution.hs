module Problems.Y2021.D04.Solution where

import Import

import Prelude (last)
import qualified Solution

solution :: Solution Input Int
solution = Solution.basic parse (uncurry part1) (uncurry part2)

type Board = [Maybe Int] -- `Nothing` represents "covered"
type Calls = [Int]
type Input = (Calls, [Board])

parse :: Parser Input
parse = (,)
  <$> decimal `sepBy` ","
  <*  "\n\n"
  <*> board `sepBy` "\n\n"
  where
    board = map Just . join <$> row `sepBy` "\n"
    row = many " " *> decimal `sepBy1` many1 " "

-- 11774
part1 :: Calls -> [Board] -> Int
part1 calls = score . winner calls
  where
    winner (n:ns) boards =
      let nexts = map (call n) boards
      in case find wins nexts of
           Just board -> (n, board)
           Nothing -> winner ns nexts
    winner [] _ = (0, mempty)

-- 4495
part2 :: Calls -> [Board] -> Int
part2 calls = score . loser calls
  where
    loser (n:ns) boards =
      let nexts = map (call n) boards
          open = filter (not . wins) nexts
      in if null open then (n, last nexts) else loser ns open
    loser [] _ = (0, mempty)

call :: Int -> Board -> Board
call n = map $ \m -> if m == Just n then Nothing else m

score :: (Int, Board) -> Int
score (n, board) = n * sum (catMaybes board)

wins :: Board -> Bool
wins = flip any lines . check

check :: Board -> [Int] -> Bool
check board = all $ (Just Nothing ==) . (board `atMay`)

lines :: [[Int]]
lines = rows <> cols
  where
    rows = [ [i, i + 1 .. i +  4] | i <- [0, 5 .. 20] ]
    cols = [ [i, i + 5 .. i + 20] | i <- [0 .. 4] ]
