module Main where

import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Options.Applicative
import Protolude hiding (option)
import Prompts (Id, fetchInput)

import Years (solutions)

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
  execParser (opts year day) >>= \case
    CommandInit (Init id) -> void $ fetchInput id
    CommandRun (Run id) -> void $ run id
    _ -> return () -- TODO: benchmarking
  where
    opts year day = info (parser year day <**> helper)
      ( fullDesc
     <> progDesc "Advent of code helpers"
     <> header "advent - helpers for advent of code"
      )

run :: (Year, Day) -> IO ()
run (year, day) = case Map.lookup year solutions of
  Nothing -> die $ "Year not implemented: " <> show year
  Just yearSolutions ->
    case Map.lookup day yearSolutions of
      Nothing -> die $ "Day not implemented: " <> show (year, day)
      Just solution -> do
        let dayNumber = "D" <> Text.justifyRight 2 '0' (show day)
            path = Text.unpack $ "src/Problems/Y" <> show year <> "/" <> dayNumber <> "/input"
        solution =<< readFile path

parser :: Year -> Day -> Parser Command
parser year day = subparser
  ( command "init"
    ( info (initP <**> helper)
      ( progDesc "Initialize a problem"
      )
    )
 <> command "run"
    ( info (runP <**> helper)
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
