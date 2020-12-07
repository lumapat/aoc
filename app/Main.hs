module Main where

import Control.Applicative (some)
import Options.Applicative
import Data.Semigroup ((<>))

data AocOptions = AocOptions
    { aocYear:: Int
    , aocDay :: Int
    , aocP1InputPath :: Maybe String
    , aocP2InputPath :: Maybe String
    }

defaultYear :: Int
defaultYear = 2020

aocOptions :: Parser AocOptions
aocOptions = AocOptions
      <$> option auto
          ( long "year"
         <> short 'y'
         <> metavar "YEAR"
         <> showDefault
         <> value defaultYear
         <> help "The year of AoC to run"
          )
      <*> option auto
          ( long "day"
         <> short 'd'
         <> metavar "DAY"
         <> help "The day to run input with"
          )
      <*> optional (strOption
          ( long "p1"
         <> short '1'
         <> metavar "PATH"
         <> help "Path for input to part one"
         ))
      <*> optional (strOption
          ( long "p2"
         <> short '2'
         <> metavar "PATH"
         <> help "Path for input to part two"
         ))

main :: IO ()
main = solve =<< execParser opts
    where opts = info (aocOptions <**> helper)
               ( fullDesc
              <> progDesc "Solve AoC questions"
              <> header "Advent of Code (AoC) solver" )


solve :: AocOptions -> IO ()
solve (AocOptions year day _ _) = print ("Hello!" ++ show year ++ " " ++ show day)
