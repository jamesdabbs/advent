module Problems.Y2022.D05.Solution where

import Import hiding (count)

import Control.Lens ((%=), at)
import qualified Data.Map as Map
import Data.Attoparsec.Text (skipSpace)
import Parsers (lines)
import qualified Solution

solution :: Solution Input String
solution = Solution.basic parse part1 part2

type Stacks = Map Int String
data Move = Move { count :: Int, source :: Int, destination :: Int } deriving (Show)

type Input = (Stacks, [Move])

parse :: Parser Input
parse = (,) <$> stacks <*> lines move
  where
    stacks = do
      columns <- fmap transpose $ lines $ crate `sepBy1` " "
      "\n"

      indices <- skipSpace *> decimal `sepBy` skipSpace <* skipSpace

      return $ Map.fromList $ zip indices $ map catMaybes columns

    crate = choice
      [ Just <$> ("[" *> anyChar <* "]")
      , "   " $> Nothing
      ]

    move = Move
      <$> ("move "  *> decimal)
      <*> (" from " *> decimal)
      <*> (" to "   *> decimal)


-- DHBJQJCCW
part1 :: Input -> String
part1 = run $ \m -> replicateM (count m) $ perform $ m { count = 1 }

-- WJVRLSJJT
part2 :: Input -> String
part2 = run perform

run :: (Move -> State Stacks a) -> Input -> String
run interpret (stacks, moves) = readout $ flip execState stacks $ forM moves interpret

pop :: Int -> State Stacks (Maybe Char)
pop n = state $ \s -> case Map.lookup n s of
  Just (c : cs) -> (Just c, Map.insert n cs s)
  _ -> (Nothing, s)

push :: Int -> Char -> State Stacks ()
push n c = at n %= fmap (c:)

perform :: Move -> State Stacks ()
perform Move{..} = do
  values <- replicateM count $ pop source
  case sequence values of
    Just vs -> mapM_ (push destination) (reverse vs)
    _       -> return ()

readout :: Stacks -> String
readout = go ""
  where
    go acc stacks = case Map.minView stacks of
      Nothing            -> reverse acc
      Just ((s:_), rest) -> go (s : acc) rest
      Just (_, rest)     -> go (' ' : acc) rest
