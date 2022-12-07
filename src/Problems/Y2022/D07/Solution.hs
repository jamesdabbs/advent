module Problems.Y2022.D07.Solution where

import Import

import Data.Attoparsec.Text (takeTill)
import qualified Data.Map as Map
import Parsers (lines)
import qualified Solution

solution :: Solution Input Integer
solution = Solution.basic parse part1 part2

type Input = [Command]

data Command
  = PopDirectory
  | PushDirectory Text
  | List [Response]
  deriving Show

data Response
  = Directory Text
  | File Integer Text
  deriving Show

parse :: Parser Input
parse = lines $ "$ " >> choice
  [ "cd .." >> pure PopDirectory
  , "cd "   >> PushDirectory <$> line
  , "ls\n"  >> List <$> lines response
  ]
  where
    line = takeTill (== '\n')
    response = choice
      [ "dir " >> Directory <$> line
      , File <$> decimal <* " " <*> line
      ]

-- 1077191
part1 :: Input -> Integer
part1 = sum . filter (<= 100000) . Map.elems . directorySizes

-- 5649896
part2 :: Input -> Integer
part2 = findTarget 70000000 30000000 . directorySizes

directorySizes :: Input -> Map [Text] Integer
directorySizes = go [] Map.empty
  where
    go _ fs [] = fs
    go path fs (PushDirectory part : rest) = go (part : path) fs rest
    go path fs (PopDirectory       : rest) = go (tail path) fs rest
    go path fs (List responses     : rest) = go path (foldr (insert path) fs responses) rest

    insert path (File size _) fs = foldr (\p -> Map.insertWith (+) p size) fs $ tails path
    insert _ _ fs = fs

findTarget :: Integer -> Integer -> Map [Text] Integer -> Integer
findTarget total unusedTarget dirs = minimum . filter (> required) $ Map.elems dirs
  where
    used = Map.findWithDefault 0 ["/"] dirs
    required = unusedTarget - (total - used)

tail :: [a] -> [a]
tail [] = []
tail (_:as) = as
