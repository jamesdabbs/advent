module Problems.Y2021.D04.Solution (solution) where

import Import

type Input = ()

solution :: Solution Input Int
solution = Solution parse $ pure . (part1 &&& part2)

parse :: Parser Input
parse = undefined

part1 :: Input -> Int
part1 = undefined

part2 :: Input -> Int
part2 = undefined
