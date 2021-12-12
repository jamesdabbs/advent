module Problems.Y2021.D12.Solution where

import Import

import Data.Attoparsec.Text (takeTill)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Set as Set
import qualified Solution

solution :: Solution Input Output
solution = Solution.basic parse part1 part2

type Input = Map Text [Text]
type Output = Int

parse :: Parser Input
parse = foldl gather mempty <$> edge `sepBy` "\n"
  where
    edge = (,) <$> takeTill (== '-') <* "-" <*> takeTill (== '\n')

    gather m (a, b) = Map.insertWith (<>) a [b] $ Map.insertWith (<>) b [a] m

part1 :: Input -> Output
part1 = run True

part2 :: Input -> Output
part2 = run False

run :: Bool -> Input -> Output
run double input = go 0 [("start", Set.singleton "start", double)]
  where
    go acc [] = acc
    go acc (path@(current, _, _) : paths)
      | current == "end" = go (acc + 1) paths
      | otherwise = go acc $ paths <> expand path

    expand (current, visited, extended) =
      let adjacents = Map.findWithDefault [] current input
          opens = filter (\n -> big n || not (Set.member n visited)) adjacents
          visitedSmalls = filter (\n -> not (big n) && Set.member n visited && n /= "start" && n /= "end") adjacents
      in map (\n -> (n, Set.insert n visited, extended)) opens
           <> (if extended then [] else map (\n -> (n, Set.insert n visited, True)) visitedSmalls)

big :: Text -> Bool
big t = Text.toUpper t == t