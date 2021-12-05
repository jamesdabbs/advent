module Utils
  ( count
  , countBy
  , dayNumber
  , firstRepeated
  , insertAt
  , manhattan
  , mapRight
  ) where

import Protolude
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.String (String)

count :: (a -> Bool) -> [a] -> Int
count condition = length . filter condition

countBy :: (Foldable t, Ord k) => (a -> k) -> t a -> Map k Int
countBy f = foldr g Map.empty
  where
    g a = Map.alter (Just . maybe 1 (+1)) (f a)

dayNumber :: Int -> Text
dayNumber = Text.justifyRight 2 '0' . show

firstRepeated :: Ord a => [a] -> Maybe a
firstRepeated ls = go ls Set.empty
  where
    go (x : xs) seen = if Set.member x seen
      then Just x
      else go xs $ Set.insert x seen
    go [] _ = Nothing

insertAt :: String -> Char -> Int -> String
insertAt (_:as) c 0 = c : as
insertAt (a:as) c n = a : insertAt as c (n - 1)
insertAt s _ _ = s

manhattan :: Num a => (a, a) -> a
manhattan (x, y) = abs x + abs y

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f (Right b) = Right (f b)
mapRight _ (Left  a) = Left a
