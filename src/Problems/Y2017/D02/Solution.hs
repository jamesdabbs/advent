module Problems.Y2017.D02.Solution where

import Import

import qualified Solution

solution :: Solution Input Output
solution = Solution.basic parse part1 part2

type Input = [[Int]]
type Output = Int

parse :: Parser Input
parse = (decimal `sepBy` "\t") `sepBy` "\n"

-- 45158
part1 :: Input -> Output
part1 = sum . map difference
  where
    difference [] = 0
    difference r = maximum r - minimum r

-- 294
part2 :: Input -> Output
part2 = sum . map divs
  where
    divs ns = [ x `divMod` y | x <- ns, y <- ns, x > y ]
      & find ((== 0) . snd)
      & maybe 0 fst
