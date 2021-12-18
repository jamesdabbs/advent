module Problems.Y2021.D18.Solution where

import Import

import Prelude (foldl1, tail)
import qualified Solution

solution :: Solution Input Output
solution = Solution.basic parse part1 part2

type Input = [Number]
type Output = Int

data Number
  = Pair Number Number
  | Regular Int
  deriving (Show, Eq)

parse :: Parser Input
parse = number `sepBy` "\n"
  where
    number = pair <|> regular

    pair = "[" >> Pair
      <$> number
      <* ","
      <*> number
      <* "]"

    regular = Regular <$> decimal

part1 :: Input -> Output
part1 = magnitude . foldl1 add

part2 :: Input -> Output
part2 input = maximum [ magnitude (add a b) | a <- input, b <- input ]

add :: Number -> Number -> Number
add a = reduce . Pair a

reduce :: Number -> Number
reduce n = maybe n reduce $ msum [tryExplode n, trySplit n]

magnitude :: Number -> Int
magnitude (Pair a b) = 3 * magnitude a + 2 * magnitude b
magnitude (Regular a) = a

data Direction = L | R
  deriving (Show, Eq, Ord)

follow :: [Direction] -> Number -> Maybe Number
follow (L : p) (Pair x _) = follow p x
follow (R : p) (Pair _ y) = follow p y
follow [] n = Just n
follow _ _ = Nothing

extendTo :: Direction -> Number -> [Direction]
extendTo d = go d []
  where
    go L acc (Pair x _) = go L (L : acc) x
    go R acc (Pair _ y) = go R (R : acc) y
    go _ acc (Regular _) = reverse acc

updateAt :: (Number -> Maybe Number) -> [Direction] -> Number -> Maybe Number
updateAt f (L : p) (Pair x y) = Pair <$> updateAt f p x <*> pure y
updateAt f (R : p) (Pair x y) = Pair <$> pure x <*> updateAt f p y
updateAt f [] n = f n
updateAt _ _ _ = Nothing

tryExplode :: Number -> Maybe Number
tryExplode n = do
  (path, l, r) <- explosion [] (0 :: Int) n

  setAt 0 path n >>= addOn L l path >>= addOn R r path
  where
    explosion acc 4 (Pair (Regular i) (Regular j)) = Just (reverse acc, i, j)
    explosion acc depth (Pair a b) = msum
      [ explosion (L : acc) (depth + 1) a
      , explosion (R : acc) (depth + 1) b
      ]
    explosion _ _ _ = Nothing

    addAt i = updateAt $ \case
      Regular j -> Just $ Regular $ i + j
      _ -> Nothing

    setAt = updateAt . const . Just . Regular

    nextPathOn d p
      | all (== d) p = Nothing
      | otherwise =
          let pre = reverse $ (d:) $ tail $ dropWhile (== d) $ reverse p
          in
            follow pre n >>= \n' ->
              Just $ pre <> extendTo (if d == L then R else L) n'

    addOn d x p = maybe Just (addAt x) $ nextPathOn d p

trySplit :: Number -> Maybe Number
trySplit (Pair a b) = case trySplit a of
  Just a' -> Just $ Pair a' b
  _ -> Pair a <$> trySplit b
trySplit (Regular v)
  | v > 9 = let l = v `div` 2 in Just $ Pair (Regular l) (Regular $ v - l)
  | otherwise = Nothing
