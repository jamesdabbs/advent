module Algo.Djikstra.Basic
  ( Graph,
    distance,
  )
where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Import

type Graph = Map Point [Point]

distance :: Ord a => Map a [a] -> a -> (a -> Bool) -> Maybe Int
distance graph start terminal = go Set.empty Map.empty 0 start
  where
    go visited acc current node
      | terminal node = Just current
      | otherwise = do
        let visited' = node `Set.insert` visited
            unvisited p = not $ p `Set.member` visited'
            unvisitedNeighbors =
              filter unvisited $ Map.findWithDefault [] node graph
            update = Just . maybe (current + 1) (min (current + 1))
            acc' = foldr (Map.alter update) acc unvisitedNeighbors
        -- TODO: more efficient priority queue implementationt
        (node', current') <- minByValue $ filter (unvisited . fst) $ Map.toList acc'
        go visited' acc' current' node'

minByValue :: Ord v => [(a, v)] -> Maybe (a, v)
minByValue [] = Nothing
minByValue ((a, v) : rest) = Just $ foldr f (a, v) rest
  where
    f x y = if snd x <= snd y then x else y
