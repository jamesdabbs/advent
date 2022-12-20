module Problems.Y2022.D13.Solution where

import Import

import Parsers (lines)
import qualified Solution

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

data Packet = PacketList [Packet] | PacketInt Int deriving (Show, Eq)
type Input = [(Packet, Packet)]

parse :: Parser Input
parse = lines $ (,) <$> packet <* "\n" <*> packet <* "\n"
  where
    packet = choice
      [ PacketInt  <$> decimal
      , PacketList <$> ("[" *> packet `sepBy` ",") <* "]"
      ]

-- 5340
part1 :: Input -> Int
part1 = sum . map fst . filter (uncurry ordered . snd) . zip [1..]

instance Ord Packet where
  compare (PacketList (a:as)) (PacketList (b:bs)) = case compare a b of
    EQ -> compare (PacketList as) (PacketList bs)
    x -> x
  compare (PacketList []) (PacketList []) = EQ
  compare (PacketList []) _ = LT
  compare _ (PacketList []) = GT

  compare (PacketInt l) (PacketInt r) = compare l r
  compare l@(PacketInt _) r = compare (PacketList [l]) r
  compare l r@(PacketInt _) = compare l (PacketList [r])

ordered :: Ord a => a -> a -> Bool
ordered a b = compare a b == LT

-- 21276
part2 :: Input -> Int
part2 input = dividers <> concatMap untuple input
  & sort
  & zip [1..]
  & filter ((`elem` dividers) . snd)
  & map fst
  & product

dividers :: [Packet]
dividers =
  [ PacketList [ PacketList [ PacketInt 2 ] ]
  , PacketList [ PacketList [ PacketInt 6 ] ]
  ]

untuple :: (a, a) -> [a]
untuple (a, b) = [a, b]
