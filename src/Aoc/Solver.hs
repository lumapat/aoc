module Aoc.Solver where

import Aoc.Input as AI
import Data.Char (isAlphaNum, isNumber, toLower)
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

solve (AI.Aoc20D3 treeRows) 1 = show $ length $ filter (==True) $ zipWith isTree [0,3..] treeRows
    where
        l = length $ head treeRows
        isTree i row = (row !! (i `mod` l)) == '#'

solve (AI.Aoc20D3 treeRows) 2 = show $ (product $ (numTrees treeRows) <$> slopes) * (numTrees evenTreeRows smallSlope)
    where
        smallSlope = [0,1..]
        evenTreeRows = fmap snd $ filter (even . fst) $ zip [0,1..] treeRows
        slopes = [smallSlope,[0,3..],[0,5..],[0,7..]]
        numTrees trees slope = length $ filter (==True) $ zipWith isTree slope trees
        l = length $ head treeRows
        isTree i row = (row !! (i `mod` l)) == '#'

solve (AI.Aoc20D4 passports) 1 = show $ length $ filter validPassport passports
    where
        validPassport = (==7) . length . filter ((/="cid") . fst)

solve (AI.Aoc20D4 passports) 2 = show $ length $ filter validPassport passports
    where
        validPassport p = (validLength p) && (all validField p)
        validLength = (==7) . length . filter ((/="cid") . fst)
        asInt v = read v :: Int
        removeSuffix s l = take (length s - l) s

        validField ("byr", v) = inRange (1920, 2002) (asInt v)
        validField ("iyr", v) = inRange (2010, 2020) (asInt v)
        validField ("eyr", v) = inRange (2020, 2030) (asInt v)
        validField ("hgt", v) | "cm" `isSuffixOf` v = inRange (150, 193) $ asInt $ removeSuffix v 2
                              | "in" `isSuffixOf` v = inRange (59, 76) $ asInt $ removeSuffix v 2
                              | otherwise          = False
        validField ("hcl", ('#':v)) = (==6) . length $ filter (isAlphaNum . toLower) v
        validField ("ecl", v) = elem v ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        validField ("pid", v) = (length v == 9) && (filter isNumber v == v)
        validField ("cid", _) = True -- Ignore
        validField _ = False

solve (AI.Aoc20D5 tickets) 1 = show $ maximum $ toSeatId <$> tickets
    where
        toSeatId t = (getRow t) * 8 + (getCol t)
        -- Courtesy of https://stackoverflow.com/a/48438340
        fromBinaryStr = foldl (\accum digit -> (fromEnum digit) + (accum * 2)) 0

        getRow = fromBinaryStr . (take 7) . (fmap (=='B'))
        getCol = fromBinaryStr . (fmap (=='R')) . (drop 7)

solve (AI.Aoc20D5 tickets) 2 = show $ head $ filter (\(x,y) -> y-x > 1) $ zip sseatIds (drop 1 sseatIds)
    where
        sseatIds = sort $ toSeatId <$> tickets
        toSeatId t = (getRow t) * 8 + (getCol t)
        -- Courtesy of https://stackoverflow.com/a/48438340
        fromBinaryStr = foldl (\accum digit -> (fromEnum digit) + (accum * 2)) 0

        getRow = fromBinaryStr . (take 7) . (fmap (=='B'))
        getCol = fromBinaryStr . (fmap (=='R')) . (drop 7)

solve (AI.Aoc20D6 groupAnswers) 1 = show $ sum $ countUniqueAnswers <$> groupAnswers
    where
        countUniqueAnswers = length . (foldr onlyUnique []) . sort . mconcat
        onlyUnique a [] = [a]
        onlyUnique a l@(x : _) | a == x    = l
                               | otherwise = a : l

solve (AI.Aoc20D6 groupAnswers) 2 = show $ sum $ countAnsByEntireGroup <$> groupAnswers
    where
        countAnsByEntireGroup g = length $ (filter ((== length g) . length)) $ (foldr chunkId []) $ sort $ mconcat g
        chunkId :: Eq a => a -> [[a]] -> [[a]]
        chunkId a []                   = [[a]]
        chunkId a (x:xs) | a == head x = (a : x) : xs
                         | otherwise   = [a] : x : xs

solve _ _ = "Invalid input sire!"