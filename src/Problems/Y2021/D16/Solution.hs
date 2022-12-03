{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Problems.Y2021.D16.Solution where

import Import hiding (yield)

import Control.Monad (fail)
import Data.Aeson (ToJSON(..))
import Data.Conduit
import qualified Data.Text as Text
import Numeric (readHex)
import qualified Solution

data Packet = Packet Int Exp
  deriving (Show, Eq, Generic, ToJSON)

data Exp
  = Sum         [Packet]
  | Product     [Packet]
  | Minimum     [Packet]
  | Maximum     [Packet]
  | Literal     Int
  | GreaterThan Packet Packet
  | LessThan    Packet Packet
  | Equal       Packet Packet
  deriving (Show, Eq, Generic, ToJSON)

solution :: Solution' String (Maybe Packet) ()
solution = Solution.basic parse part1 part2

parse :: Parser String
parse = many1 $ notChar '\n'

part1 :: String -> Maybe Packet
part1 input = runConduit $ mapM_ yield input .| hexToBinary .| packet
  where
    hexToBinary = awaitForever $ mapM_ yield . bits

    packet :: Monad m => ConduitT Int Void m Packet
    packet = do
      version <- readBits 3
      typeId <- readBits 3
      return $ Packet version $ Literal typeId

    readBits :: Monad m => Int -> ConduitT Int Void m Int
    readBits n = do
      bs <- replicateM n $ fmap (fromMaybe 0) await
      return $ asBase 2 bs

part2 :: String -> ()
part2 input = ()

toPacket :: [Int16] -> Packet
toPacket _ = Packet 1 (Literal 1)

bits :: Char -> [Int]
bits '0' = [0, 0, 0, 0]
bits '1' = [0, 0, 0, 1]
bits '2' = [0, 0, 1, 0]
bits '3' = [0, 0, 1, 1]
bits '4' = [0, 1, 0, 0]
bits '5' = [0, 1, 0, 1]
bits '6' = [0, 1, 1, 0]
bits '7' = [0, 1, 1, 1]
bits '8' = [1, 0, 0, 0]
bits '9' = [1, 0, 0, 1]
bits 'A' = [1, 0, 1, 0]
bits 'B' = [1, 0, 1, 1]
bits 'C' = [1, 1, 0, 0]
bits 'D' = [1, 1, 0, 1]
bits 'E' = [1, 1, 1, 0]
bits  _  = [1, 1, 1, 1]

-- hexDigitToInt :: Char -> Int16
-- hexDigitToInt c
--   | c >= '0' && c <= '9' = fromIntegral $ ord c - ord '0'
--   | c >= 'a' && c <= 'f' = fromIntegral $ ord c - (ord 'a' - 10)
--   | otherwise            = fromIntegral $ ord c - (ord 'A' - 10)