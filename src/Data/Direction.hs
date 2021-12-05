module Data.Direction
  ( Direction(..)
  ) where

import Protolude

data Direction = N | E | S | W
  deriving (Show, Eq, Ord, Enum, Bounded)
