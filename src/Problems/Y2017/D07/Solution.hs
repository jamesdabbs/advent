module Problems.Y2017.D07.Solution where

import Import

import qualified Data.Set as Set
import Data.Attoparsec.Text (takeTill)
import Parsers (word)
import qualified Solution

solution :: Solution' Input Output Output'
solution = Solution.basic parse part1 part2

type Input = [(Text, Int, [Text])]
type Output = Maybe Text
type Output' = Text

parse :: Parser Input
parse = link `sepBy` "\n"
  where
    link = (,,)
      <$> takeTill (== ' ')
      <* " ("
      <*> decimal
      <* ")"
      <*> ( (" -> " *> (word `sepBy` ", ")) <|> pure [] )

part1 :: Input -> Output
part1 = findMissing . foldl note (mempty, mempty)
  where
    note (bases, tops) (name, _, supported) =
      (Set.insert name bases, Set.union tops $ Set.fromList supported)

    findMissing (bases, tops) = head $ Set.toList $ bases Set.\\ tops

part2 :: Input -> Output'
part2 _ = "TODO"
