module Problems.Y2022.D21.Solution where

import Data.Attoparsec.Text (take)
import qualified Data.Map as Map
import Import hiding (Const, evaluate, take, takeWhile)
import Parsers (lines)
import qualified Solution

solution :: Solution Input Int
solution = Solution.basic parse part1 part2

type Ref = Text

-- Supported binary operators
data Operator = Add | Sub | Mult | Div
  deriving (Show, Eq)

-- An expression AST
data Exp
  = Const Int
  | Op Exp Operator Exp
  | Unknown
  deriving (Show, Eq)

-- Raw input lines, to be used to build an expression tree
data InputLine
  = LineConst Int
  | LineOp Ref Operator Ref
  | {-
      KLUDGE: while the input file doesn't ever contain a literal Unknown, the
      Exp structure erases the ref labels, so we can't directly patch the
      `humn` node. Rather than plumbing through that metadata, it's easier to
      patch inputs before building the tree, so we also have an Unknown type here.
    -}
    LineUnknown
  deriving (Show, Eq)

type Input = Map Ref InputLine

parse :: Parser Input
parse =
  fmap Map.fromList . lines $
    (,)
      <$> take 4
      <* ": "
      <*> (constant <|> operation)
  where
    constant = LineConst <$> decimal

    operation =
      LineOp
        <$> take 4
        <* " "
        <*> operator
        <* " "
        <*> take 4

    operator =
      choice
        [ "+" $> Add,
          "-" $> Sub,
          "*" $> Mult,
          "/" $> Div
        ]

-- 170237589447588
part1 :: Input -> Int
part1 input =
  let Right (Const value) = reduce <$> toExp "root" input
   in value

-- 3712643961892
part2 :: Input -> Int
part2 input =
  let Right (Op a _ b) = toExp "root" $ Map.insert "humn" LineUnknown input
      Right value = case (reduce a, reduce b) of
        (Const n, b') -> set n b'
        (a', Const n) -> set n a'
        unsolvable -> Left $ "Not solveable: " <> show unsolvable
   in value

-- Attempt to weave the ref-based Input map into a Exp AST, with ref checking
toExp :: Ref -> Input -> Either Ref Exp
toExp ref lookup = case Map.lookup ref lookup of
  Just (LineConst i) -> Right $ Const i
  Just (LineOp a o b) -> Op <$> toExp a lookup <*> Right o <*> toExp b lookup
  Just LineUnknown -> Right Unknown
  Nothing -> Left ref

-- Simplify an Exp AST as much as possible, preserving Unknowns
reduce :: Exp -> Exp
reduce (Op a o b) = case (reduce a, reduce b) of
  (Const a', Const b') -> Const $ op o a' b'
  (a', b') -> Op a' o b'
reduce e = e

-- Given a reduced AST with a single Unknown, find the value of the Unknown
-- which causes the AST to evaluate to the target value. Errors if there is not
-- exactly one Unknown in the tree.
set :: Int -> Exp -> Either Text Int
set n Unknown = Right n
set n (Op a o (Const b)) = set (rightInvert o n b) a
set n (Op (Const a) o b) = set (leftInvert o n a) b
set _ o = Left $ "Cannot set value for " <> show o

op :: Operator -> Int -> Int -> Int
op Add = (+)
op Sub = (-)
op Mult = (*)
op Div = div

-- rightInvert op n a = b => n = a `op` b
rightInvert :: Operator -> Int -> Int -> Int
rightInvert Add = (-)
rightInvert Sub = (+)
rightInvert Mult = div
rightInvert Div = (*)

-- leftInvert op n b = a => n = a `op` b
leftInvert :: Operator -> Int -> Int -> Int
leftInvert Add = (-)
leftInvert Sub = flip (-)
leftInvert Mult = div
leftInvert Div = flip div
