module Main where

import qualified Aoc.Input as AI
import Aoc.Solver

import Control.Applicative (some)
import Options.Applicative
import Data.Semigroup ((<>))

data AocOptions = AocOptions
    { aocYear:: Int
    , aocDay :: Int
    , aocPart :: Int
    , aocInputPath :: String
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
      <*> option auto
          ( long "part"
         <> short 'p'
         <> showDefault
         <> value 1
         <> help "Part/section of the day to solve"
          )
      <*> strOption
          ( long "input-file"
         <> short 'i'
         <> metavar "PATH"
         <> help "Path for input"
          )

main :: IO ()
main = actualMain =<< execParser opts
    where opts = info (aocOptions <**> helper)
               ( fullDesc
              <> progDesc "Solve AoC questions"
              <> header "Advent of Code (AoC) solver" )

preamble :: Int -> Int -> Int -> String
preamble y d p = "Answer to AOC " ++ show y ++ " Day " ++ show d ++ " Part " ++ show p ++ " is: "

actualMain :: AocOptions -> IO ()
actualMain (AocOptions y d part path) = do
    s <- readFile path
    case AI.parseAocInput y d s of
        Right input -> putStrLn $ preamble y d part ++ solve input part
        Left s -> putStrLn s