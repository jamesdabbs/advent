module Problems.Y2021.D03.Solution (solution) where

import Import
import Data.Attoparsec.Text (digit, many', sepBy1)
import Data.List (partition)
import qualified Data.Matrix as Matrix
import qualified Data.Vector as Vector

data Input = Input
  { width :: Int
  , values :: [Int]
  }

solution :: Solution Input (Maybe Int)
solution = Solution parse $ pure . (Just . part1 &&& part2)

parse :: Parser Input
parse = do
  let bit = ("0" $> 0 <|> "1" $> 1)
  values@(first:_) <- many' bit `sepBy1` "\n"
  return $ Input (length first) (map binpack values)

binpack :: [Int] -> Int
binpack = reverse
  >>> zip [0..]
  >>> map (\(i, v) -> v * (2^i))
  >>> sum

-- 281 * 3814 = 1071734
part1 :: Input -> Int
part1 (Input width values) = uncurry (*) $ foldl f (0, 0) [0 .. width - 1]
  where
    f (gamma, epsilon) i =
      let (zeros, ones) = partition (== 0) $ map (.&. bit i) values
      in if length ones > length zeros
           then (gamma .|. bit i, epsilon)
           else (gamma, epsilon .|. bit i)

-- 1679 * 3648 = 6124992
part2 :: Input -> Maybe Int
part2 (Input width values) = (*)
  <$> go (>=) width values
  <*> go (< ) width values
  where
    go _ _ [value] = Just value
    go _ 0 _ = Nothing
    go cmp i vs =
      let (ones, zeros) = partition (`testBit` (i - 1)) vs
      in go cmp (i - 1) $ if length ones `cmp` length zeros then ones else zeros
