{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import Data.String (String)
import qualified Data.Text as Text
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Options.Applicative
import Protolude hiding (option)
import Prompts (Id, fetchInput)

import qualified Problems.Y2017 as Y2017
import qualified Problems.Y2021 as Y2021

type Year = Int
type Day = Int

data Init = Init Id
data Run = Run Id
data Benchmark = Benchmark Id

data Command
  = CommandInit Init
  | CommandRun Run
  | CommandBenchmark Benchmark

main :: IO ()
main = do
  (year, day) <- activeDate
  command <- execParser $ opts year day
  case command of
    CommandInit (Init id) -> void $ fetchInput id
    CommandRun (Run id) -> void $ run id
    CommandBenchmark (Benchmark id) -> putStrLn ("TODO: bench" :: Text)
  where
    opts year day = info (parser year day <**> helper)
      ( fullDesc
     <> progDesc "Advent of code helpers"
     <> header "advent - helpers for advent of code"
      )

run :: (Year, Day) -> IO ()
run (year, day) = do
  solutions <- case year of
    2017 -> return Y2017.solutions
    2021 -> return Y2021.solutions
    _ -> die $ "Year not implemented: " <> show year
  case Map.lookup day solutions of
    Just solution -> do

      raw <- readFile $ Text.unpack $ "src/Problems/Y" <> show year <> "/D" <> (Text.justifyRight 2 '0' $ show day) <> "/input"
      solution raw
    _ -> die $ "Day not implemented: " <> show (year, day)

parser :: Year -> Day -> Parser Command
parser year day = subparser
  ( command "init"
    ( info initP
      ( progDesc "Initialize a problem"
      )
    )
 <> command "run"
    ( info runP
      ( progDesc "Run a solution"
      )
    )
  )
  where
    idP = (,)
      <$> option auto (long "year" <> short 'y' <> metavar "YEAR" <> value year)
      <*> option auto (long "day"  <> short 'd' <> metavar "DAY"  <> value day)

    initP = CommandInit . Init <$> idP
    runP = CommandRun . Run <$> idP

activeDate :: IO (Year, Day)
activeDate = do
  (year, month, day) <- toGregorian . utctDay <$> getCurrentTime
  return $ if month == 12
    then (fromIntegral year, day)
    else (fromIntegral (year - 1), day)
