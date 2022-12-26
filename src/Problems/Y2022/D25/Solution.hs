module Problems.Y2022.D25.Solution where

import Data.Attoparsec.Text (char)
import qualified Data.Map as Map
import Import hiding (digit)
import Parsers (lines)
import qualified Solution

solution :: Solution Input String
solution = Solution.basic parse part1 (pure "")

newtype Snafu = Snafu {digits :: [Int]} deriving (Show, Eq)

type Input = [Snafu]

parse :: Parser Input
parse = lines $ Snafu . reverse <$> many1 digit
  where
    digit = choice $ map (\(c, v) -> char c $> v) symbols

symbols :: [(Char, Int)]
symbols =
  [ ('2', 2),
    ('1', 1),
    ('0', 0),
    ('-', -1),
    ('=', -2)
  ]

-- 2-0-0=1-0=2====20=-2
part1 :: Input -> String
part1 = map toDecimal >>> sum >>> fromDecimal >>> format

toDecimal :: Snafu -> Int
toDecimal = go 0 1 . digits
  where
    go acc _ [] = acc
    go acc base (n : ns) = go (acc + n * base) (base * 5) ns

fromDecimal :: Int -> Snafu
fromDecimal = Snafu . go []
  where
    go acc 0 = acc
    go acc value =
      let (q, r) = value `divMod` 5
       in if r >= 3 then go (r - 5 : acc) (q + 1) else go (r : acc) q

format :: Snafu -> String
format = map f . digits
  where
    lookup = Map.fromList $ map swap symbols

    f s = Map.findWithDefault '?' s lookup
