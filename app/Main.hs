module Main where

import Options.Applicative
import Data.Semigroup ((<>))

data AocOptions = AocOptions
    { aocYear:: Int
    , aocDay :: Int
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
         <> help "The year of AoC to run" )
      <*> option auto
          ( long "day"
         <> short 'd'
         <> metavar "DAY"
         <> help "The day to run input with" )

main :: IO ()
main = doThings =<< execParser opts
    where opts = info (aocOptions <**> helper)
               ( fullDesc
              <> progDesc "Solve AoC questions"
              <> header "Advent of Code (AoC) solver" )


doThings :: AocOptions -> IO ()
doThings (AocOptions year day) = print ("Hello!" ++ show year ++ " " ++ show day)
