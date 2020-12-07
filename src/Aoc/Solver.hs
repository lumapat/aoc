module Aoc.Solver where

import Aoc.Input as AI
import Data.Ix (inRange)
import Data.List

solve :: AI.AocInput -> Int -> String
solve (AI.Aoc20D1 ints) 1 = case head candidates of
        [x,y] -> show (x*y)
        _ -> "This input doesn't work at all"
    where
        candidates = filter ((2020==) . sum) $ filter ((2==) . length . nub) $ mapM (const ints) [1..2]

solve (AI.Aoc20D1 ints) 2 = case head candidates of
        [x,y,z] -> show (x*y*z)
        _ -> "This input doesn't work at all"
    where
        candidates = filter ((2020==) . sum) $ filter ((3==) . length . nub) $ mapM (const ints) [1..3]

solve (AI.Aoc20D2 passwords) 1 = show $ length $ filter validPassword passwords
    where
        validPassword (AI.Password range c p) = inRange range (length $ filter (==c) p)

solve (AI.Aoc20D2 passwords) 2 = show $ length $ filter validPassword passwords
    where
        validPassword (AI.Password (i, i') c p) = lower /= upper
            where
                lower = (p !! (i-1)) == c
                upper = (p !! (i'-1)) == c

solve _ _ = "Invalid input sire!"