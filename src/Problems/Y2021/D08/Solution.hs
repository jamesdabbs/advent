module Problems.Y2021.D08.Solution where

import Import hiding (evaluate)

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Set as Set
import Parsers (readEnumBy)
import qualified Solution

solution :: Solution Input Output
solution = Solution.basic parse part1 part2

type Input = [Entry]
type Output = Int

data Segment = A | B | C | D | E | F | G
  deriving (Show, Eq, Ord, Enum, Bounded)

type Pattern = Set Segment

type Permutation = [Segment]

data Entry = Entry
  { ins  :: [Pattern]
  , outs :: [Pattern]
  } deriving (Show)

parse :: Parser Input
parse = line `sepBy` "\n"
  where
    line = Entry
      <$> pattern `sepBy` " "
      <* " | "
      <*> pattern `sepBy` " "
    pattern = Set.fromList <$> many1 (readEnumBy Text.toLower)

part1 :: Input -> Output
part1 = sum . map (count rightSized . outs)
  where
    rightSized pattern = Set.size pattern `elem` [2, 4, 3, 7]

part2 :: Input -> Output
part2 = sum . map (fromJust . evaluate)

evaluate :: Entry -> Maybe Int
evaluate Entry{..} = unscrambler >>= apply outs
  where
    unscrambler :: Maybe Permutation
    unscrambler = find (allValid ins) $ permutations [A .. G]

    apply :: [Pattern] -> Permutation -> Maybe Int
    apply pattern permutation = do
      digits <- sequence $ map (readWith permutation) pattern
      return $ asBase 10 digits

segmentsUsed :: Int -> Pattern
segmentsUsed 0 = Set.fromList [A, B, C,    E, F, G]
segmentsUsed 1 = Set.fromList [      C,       F   ]
segmentsUsed 2 = Set.fromList [A,    C, D, E,    G]
segmentsUsed 3 = Set.fromList [A,    C, D,    F, G]
segmentsUsed 4 = Set.fromList [   B, C, D,    F   ]
segmentsUsed 5 = Set.fromList [A, B,    D,    F, G]
segmentsUsed 6 = Set.fromList [A, B,    D, E, F, G]
segmentsUsed 7 = Set.fromList [A,    C,       F   ]
segmentsUsed 8 = Set.fromList [A, B, C, D, E, F, G]
segmentsUsed 9 = Set.fromList [A, B, C, D,    F, G]
segmentsUsed _ = mempty

allValid :: [Pattern] -> Permutation -> Bool
allValid patterns permutation = all isJust $ map (readWith permutation) patterns

numerals :: Map (Set Segment) Int
numerals = Map.fromList $ map (\n -> (segmentsUsed n, n)) [0 .. 9]

readWith :: Permutation -> Pattern -> Maybe Int
readWith permutation pattern =
  let index = Map.fromList $ zip [A .. G] permutation
      translated = Set.map (\r -> fromJust $ Map.lookup r index) pattern
  in Map.lookup translated numerals
