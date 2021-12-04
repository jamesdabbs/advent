module Problems.Y2021.D03.Solution where

import Import
import qualified Solution

data Input = Input
  { width :: Int
  , values :: [Int]
  }

solution :: Solution Input (Maybe Int)
solution = Solution.basic parse (Just . part1) part2

parse :: Parser Input
parse = do
  values@(v:_) <- many' b `sepBy1` "\n"
  return $ Input (length v) (map binpack values)
  where
    b = "0" $> 0 <|> "1" $> 1

binpack :: [Int] -> Int
binpack = reverse
  >>> zip [0..]
  >>> map (\(i, v) -> v * bit i)
  >>> sum

-- 281 * 3814 = 1071734
part1 :: Input -> Int
part1 Input{..} = uncurry (*) $ foldl f (0, 0) [0 .. width - 1]
  where
    f (gamma, epsilon) i =
      let (zeros, ones) = partition (== 0) $ map (.&. bit i) values
      in if length ones > length zeros
           then (gamma .|. bit i, epsilon)
           else (gamma, epsilon .|. bit i)

-- 1679 * 3648 = 6124992
part2 :: Input -> Maybe Int
part2 Input{..} = (*)
  <$> go (>=) width values
  <*> go (< ) width values
  where
    go _ _ [value] = Just value
    go _ 0 _ = Nothing
    go cmp i vs =
      let (ones, zeros) = partition (`testBit` (i - 1)) vs
      in go cmp (i - 1) $ if length ones `cmp` length zeros then ones else zeros
