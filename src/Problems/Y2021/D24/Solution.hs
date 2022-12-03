module Problems.Y2021.D24.Solution where

import Import hiding (get)

import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..))
import qualified Solution

data Variable = W | X | Y | Z deriving (Show, Eq, Ord)

data ALU = ALU
  { w :: !Int
  , x :: !Int
  , y :: !Int
  , z :: !Int
  } deriving (Show, Eq)

data Instruction
  = Inp Variable
  | Add Variable (Either Variable Int)
  | Mul Variable (Either Variable Int)
  | Div Variable (Either Variable Int)
  | Mod Variable (Either Variable Int)
  | Eql Variable (Either Variable Int)
  deriving (Show, Eq)

solution :: Solution' Input Output Output'
solution = Solution.basic parse part1 part2

type Input = [Instruction]
type Output = Maybe [Int]
type Output' = Output

parse :: Parser Input
parse = instruction `sepBy` "\n"
  where
    instruction = choice
      [ "inp " >> Inp <$> variable
      , "add " >> Add <$> variable <* " " <*> value
      , "mul " >> Mul <$> variable <* " " <*> value
      , "div " >> Div <$> variable <* " " <*> value
      , "mod " >> Mod <$> variable <* " " <*> value
      , "eql " >> Eql <$> variable <* " " <*> value
      ]

    variable = choice
      [ "w" $> W
      , "x" $> X
      , "y" $> Y
      , "z" $> Z
      ]

    value = fmap Left variable <|> fmap Right (signed decimal)

type S = (ALU, [Instruction], [Int])

part1 :: Input -> Output
part1 rom = dfs $ Seq.singleton (ALU 0 0 0 0, rom, [])
  where
    dfs ((ALU{..}, [], inputs) :<| states)
      | z == 0    = Just $ reverse inputs
      | otherwise = dfs states
    dfs (s :<| states) = dfs $ nexts s <> states
    dfs _ = Nothing

    nexts (alu, instructions, inputs) = Seq.fromList $ flip map [9, 8 .. 1] $ \n ->
      let (alu', instructions') = consume instructions n alu
      in (alu', instructions', n : inputs)

consume :: [Instruction] -> Int -> ALU -> (ALU, [Instruction])
consume [] _ alu = (alu, [])
consume (x:xs) i alu =
  let alu' = exec x i alu
  in case x of
       Inp _ -> (alu', xs)
       _ -> consume xs i alu'

exec :: Instruction -> Int -> ALU -> ALU
exec (Inp v  ) i = update v $ const i
exec (Add v r) _ = updateWith v r (+)
exec (Mul v r) _ = updateWith v r (*)
exec (Div v r) _ = updateWith v r div
exec (Mod v r) _ = updateWith v r mod
exec (Eql v r) _ = updateWith v r $ \a b -> if a == b then 1 else 0

get :: Variable -> ALU -> Int
get W ALU{..} = w
get X ALU{..} = x
get Y ALU{..} = y
get Z ALU{..} = z

update :: Variable -> (Int -> Int) -> ALU -> ALU
update W f ALU{..} = ALU{ w = f w, x, y, z }
update X f ALU{..} = ALU{ w, x = f x, y, z }
update Y f ALU{..} = ALU{ w, x, y = f y, z }
update Z f ALU{..} = ALU{ w, x, y, z = f z }

updateWith :: Variable -> Either Variable Int -> (Int -> Int -> Int) -> ALU -> ALU
updateWith a (Left  b) f alu = update a (flip f $ get b alu) alu
updateWith a (Right b) f alu = update a (flip f b) alu

part2 :: Input -> Output'
part2 input = part1 input
