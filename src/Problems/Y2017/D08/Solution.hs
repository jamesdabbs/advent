module Problems.Y2017.D08.Solution where

import Import hiding (EQ, LT, GT)

import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Set as Set
import Data.Attoparsec.Text (takeTill)
import qualified Solution

solution :: Solution' Input Output ()
solution = Solution.basic parse part1 part2

type Instruction = (Register, Action, Register, Condition)

type Register = Text
data Action = Inc Int | Dec Int
  deriving (Show, Eq)
data Condition
  = GT Int
  | LT Int
  | GTE Int
  | LTE Int
  | EQ Int
  | NEQ Int
  deriving (Show, Eq)
type Registers = Map Register Int

type Input = [Instruction]
type Output = Int

parse :: Parser Input
parse = instruction `sepBy` "\n"
  where
    instruction = (,,,)
      <$> takeTill (== ' ')
      <* " "
      <*> action
      <* " if "
      <*> takeTill (== ' ')
      <* " "
      <*> condition

    action = choice
      [ "inc " >> fmap Inc (signed decimal)
      , "dec " >> fmap Dec (signed decimal)
      ]

    condition = choice
      [ "> " >> fmap GT (signed decimal)
      , "< " >> fmap LT (signed decimal)
      , ">= " >> fmap GTE (signed decimal)
      , "<= " >> fmap LTE (signed decimal)
      , "== " >> fmap EQ (signed decimal)
      , "!= " >> fmap NEQ (signed decimal)
      ]

part1 :: Input -> Output
part1 = largestValue . foldr eval mempty

part2 :: Input -> ()
part2 input = ()

eval :: Instruction -> Registers -> Registers
eval i@(dst, act, src, cond) acc =
  if check cond src $ traceShow (acc, i) acc
    then apply act dst acc
    else acc

apply :: Action -> Register -> Registers -> Registers
apply (Inc n) r = Map.insertWith (+) r n
apply (Dec n) r = Map.insertWith (flip (-)) r n

check :: Condition -> Register -> Registers -> Bool
check (GT n) = check' (> n)
check (LT n) = check' (< n)
check (GTE n) = check' (>= n)
check (LTE n) = check' (<= n)
check (EQ n) = check' (== n)
check (NEQ n) = check' (/= n)

check' f r = f . Map.findWithDefault 0 r

largestValue = maximum . Map.elems