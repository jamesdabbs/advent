module Problems.Y2021.D04.Solution (solution) where

import Import hiding (check, lines)

import Prelude (last)

solution :: Solution Input Int
solution = Solution parse $ pure . (part1 &&& part2)

type Board = [Maybe Int] -- `Nothing` represents "covered"
type Input = ([Int], [Board])

parse :: Parser Input
parse = (,)
  <$> decimal `sepBy` ","
  <*  "\n\n"
  <*> board `sepBy` "\n\n"
  where
    board = map Just . join <$> row `sepBy` "\n"
    row = many " " *> decimal `sepBy1` many1 " "

-- 11774
part1 :: Input -> Int
part1 (calls, boards) = score $ winner calls boards
  where
    winner (n:ns) boards =
      let nexts = map (call n) boards
      in case find wins nexts of
           Just board -> (n, board)
           Nothing -> winner ns nexts

-- 4495
part2 :: Input -> Int
part2 (calls, boards) = score $ loser calls boards
  where
    loser (n:ns) boards =
      let nexts = map (call n) boards
          open = filter (not . wins) nexts
      in if null open then (n, last nexts) else loser ns open

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
