module Utils
  ( count
  , countBy
  , firstRepeated
  , insertAt
  ) where

import Protolude
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.String (String)

count :: (a -> Bool) -> [a] -> Int
count condition = length . filter condition

countBy :: (Foldable t, Ord k) => (a -> k) -> t a -> Map k Int
countBy f = foldr g Map.empty
  where
    g a = Map.alter (Just . maybe 1 (+1)) (f a)

firstRepeated :: Ord a => [a] -> Maybe a
firstRepeated list = go list Set.empty
  where
    go (x : xs) seen = if Set.member x seen
      then Just x
      else go xs (Set.insert x seen)
    go [] _ = Nothing

insertAt :: String -> Char -> Int -> String
insertAt (a:as) c 0 = c : as
insertAt (a:as) c n = a : insertAt as c (n - 1)
insertAt s _ _ = s
