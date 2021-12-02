module Utils
  ( count
  ) where

import Protolude

count :: (a -> Bool) -> [a] -> Int
count condition = length . filter condition
