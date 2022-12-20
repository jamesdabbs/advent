module Problems.Y2022.D10.Solution where

import Import

import Control.Monad.Trans.Writer (Writer, execWriter, tell)
import Parsers (lines)
import qualified Solution

solution :: Solution' Input Int String
solution = Solution.basic parse part1 part2

data Command
  = NoOp
  | AddX Int
  deriving (Show, Eq)

type Input = [Command]

parse :: Parser Input
parse = lines $ choice
  [ "noop" $> NoOp
  , "addx " >> AddX <$> signed decimal
  ]

-- 11220
part1 :: Input -> Int
part1 = sum . run observe
  where
    observe n x = when (n `mod` 40 == 20) $ tell [n * x]

-- BZPAJELK
part2 :: Input -> String
part2 = ('\n':) . run render
  where
    render n x = do
      let n' = (n - 1) `mod` 40
      tell $ if x - 1 <= n' && n' <= x + 1 then "█" else " "
      when (n `mod` 40 == 0) $ tell "\n"

run :: Monoid a => (Int -> Int -> Writer a b) -> [Command] -> a
run handler = execWriter . go 1 1
  where
    go n x (NoOp : cmds) =
      handler n x >> go (n + 1) x cmds
    go n x (AddX v : cmds) =
      handler n x >> handler (n + 1) x >> go (n + 2) (x + v) cmds
    go _ _ [] = pure ()
