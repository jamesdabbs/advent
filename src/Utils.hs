module Utils
  ( asBase
  , count
  , countBy
  , dayNumber
  , firstRepeated
  , firstRepeated'
  , insertAt
  , manhattan
  , mapLeft
  , mapRight
  , pairs
  ) where

import Protolude
import Control.Arrow ((>>>))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.String (String)

asBase :: Int -> [Int] -> Int
asBase b = reverse >>> zip [(0 :: Int) ..] >>> map (\(i, v) -> v * b ^ i) >>> sum

count :: (a -> Bool) -> [a] -> Int
count condition = length . filter condition

countBy :: (Foldable t, Ord k) => (a -> k) -> t a -> Map k Int
countBy f = foldr g Map.empty
  where
    g a = Map.alter (Just . maybe 1 (+1)) (f a)

dayNumber :: Int -> Text
dayNumber = Text.justifyRight 2 '0' . show

firstRepeated :: Ord a => [a] -> Maybe a
firstRepeated ls = fst <$> firstRepeated' ls

firstRepeated' :: Ord a => [a] -> Maybe (a, Int)
firstRepeated' ls = go ls 0 Set.empty
  where
    go (x : xs) n seen = if Set.member x seen
      then Just (x, n)
      else go xs (n + 1) $ Set.insert x seen
    go [] _ _ = Nothing

insertAt :: String -> Char -> Int -> String
insertAt (_:as) c 0 = c : as
insertAt (a:as) c n = a : insertAt as c (n - 1)
insertAt s _ _ = s

manhattan :: Num a => (a, a) -> a
manhattan (x, y) = abs x + abs y

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft _ (Right b) = Right b
mapLeft f (Left  a) = Left (f a)

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f (Right b) = Right (f b)
mapRight _ (Left  a) = Left a

-- From Y2021D01
pairs :: [a] -> [(a, a)]
pairs (x : y : zs) = (x, y) : pairs (y : zs)
pairs _ = []
