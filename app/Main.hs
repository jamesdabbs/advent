module Main where

import Api (submitAnswer)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Time.Calendar (toGregorian)
import Data.Time.Clock (getCurrentTime, utctDay)
import Options.Applicative
import Problems.Problems (solutions)
import Prompts (Id, initialize, persistPrompt)
import Protolude hiding (option)

type Year = Int

type Day = Int

data Options = Options Id Command

data Run = Run
  { runSubmit :: Maybe Int,
    useExample :: Bool
  }

data Command
  = CommandInit
  | CommandRun Run
  | CommandBenchmark

main :: IO ()
main = do
  (year, day) <- activeDate
  execParser (opts year day) >>= \case
    Options id CommandInit -> initialize id
    Options id (CommandRun Run {..}) -> void $ run runSubmit useExample id
    _ -> return () -- TODO: benchmarking
  where
    opts year day =
      info
        (parser year day <**> helper)
        ( fullDesc
            <> progDesc "Advent of code helpers"
            <> header "advent - helpers for advent of code"
        )

run :: Maybe Int -> Bool -> (Year, Day) -> IO ()
run sub useExample (year, day) = case Map.lookup (year, day) solutions of
  Nothing -> die $ "Not implemented: " <> show (year, day)
  Just solution -> do
    let dayNumber = "D" <> Text.justifyRight 2 '0' (show day)
        suffix = if useExample then ".example" else ""
        path = Text.unpack $ "src/Problems/Y" <> show year <> "/" <> dayNumber <> "/input" <> suffix
    (s1, s2) <- solution =<< readFile path
    case sub of
      Just 1 -> do
        either die putStrLn =<< submitAnswer year day 1 s1
        void $ persistPrompt year day
      Just 2 -> do
        either die putStrLn =<< submitAnswer year day 2 s2
        void $ persistPrompt year day
      _ -> return ()

parser :: Year -> Day -> Parser Options
parser year day =
  Options
    <$> idP
    <*> subparser
      ( command
          "init"
          ( info
              (initP <**> helper)
              ( progDesc "Initialize a problem"
              )
          )
          <> command
            "run"
            ( info
                (runP <**> helper)
                ( progDesc "Run a solution"
                )
            )
      )
  where
    idP =
      (,)
        <$> option auto (long "year" <> short 'y' <> metavar "YEAR" <> value year)
        <*> option auto (long "day" <> short 'd' <> metavar "DAY" <> value day)

    initP = pure CommandInit

    runP =
      fmap CommandRun $
        Run
          <$> optional (option auto (long "submit" <> short 's' <> metavar "Submit"))
          <*> switch (long "example" <> short 'e' <> help "Use input.example")

activeDate :: IO (Year, Day)
activeDate = do
  (year, month, day) <- toGregorian . utctDay <$> getCurrentTime
  return $
    if month == 12
      then (fromIntegral year, day)
      else (fromIntegral (year - 1), day)
