module Aoc.Solver where

import Aoc.Input as AI
import Data.Char (isAlphaNum, isNumber, toLower)
import qualified Data.HashSet as HS
import Data.Ix (inRange)
import Data.List
import qualified Data.Map as M

-- Q1
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

-- Q2
solve (AI.Aoc20D2 passwords) 1 = show $ length $ filter validPassword passwords
    where
        validPassword (AI.Password range c p) = inRange range (length $ filter (==c) p)

solve (AI.Aoc20D2 passwords) 2 = show $ length $ filter validPassword passwords
    where
        validPassword (AI.Password (i, i') c p) = lower /= upper
            where
                lower = (p !! (i-1)) == c
                upper = (p !! (i'-1)) == c

-- Q3
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

-- Q4
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

-- Q5
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

-- Q6
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

-- Q7
solve (AI.Aoc20D7 bags) 1 = show $ length
                                 $ filter ((/= "shiny gold") . fst)
                                 $ filter willContainShinyGold bags
    where
        rules = M.fromList bags
        findRules color = (color, M.findWithDefault [] color rules)

        willContainShinyGold :: (String, [(String, Int)]) -> Bool
        willContainShinyGold (_, [])           = False
        willContainShinyGold ("shiny gold", _) = True
        willContainShinyGold (color, items)    = any willContainShinyGold $ (findRules . fst) <$> items

solve (AI.Aoc20D7 bags) 2 = show $ findCount "shiny gold"
    where
        rules = M.fromList bags
        findCount color = sum $ convertToCount <$> (M.findWithDefault [] color rules)
        convertToCount (color, quantity) = quantity + quantity * (findCount color)

-- Q8
solve (AI.Aoc20D8 instructions) 1 = show $ accumulateBeforeLoop (0,0) lineToInstruction HS.empty
    where
        lineToInstruction = M.fromList $ zip [0..] instructions

solve (AI.Aoc20D8 instructions) 2 = show $ accumulateBeforeLoop (0,0) (singleMapFlip terminatingLine) HS.empty
    where
        l2i = M.fromList $ zip [0..] instructions
        terminatingLine = head $ filter (terminates . singleMapFlip) [0..(length instructions)]
        singleMapFlip l = M.adjust flipInstruction l l2i
        flipInstruction ("jmp", v) = ("nop", v)
        flipInstruction ("nop", v) = ("jmp", v)
        flipInstruction x          = x

solve _ _ = "Invalid input sire!"

-- Q8
evalInstruction :: (String, Int) -> (Int, Int) -> (Int, Int)
evalInstruction ("acc",val) (acc,l) = (acc+val,l+1)
evalInstruction ("jmp",val) (acc,l) = (acc,l+val)
evalInstruction (_,_) (acc, l) = (acc,l+1) -- includes "nop" case

terminates :: M.Map Int (String, Int) -> Bool
terminates l2i = f' 0 l2i HS.empty
    where
        f' l l2i' visited
            | HS.member l visited  = False
            | not (M.member l l2i) = True
            | otherwise            = f' l' l2i' newVisited
                where
                   l' = snd $ evalInstruction (l2i M.! l) (0,l)
                   newVisited = HS.insert l visited


accumulateBeforeLoop :: (Int, Int) -> M.Map Int (String, Int) -> HS.HashSet Int -> Int
accumulateBeforeLoop (accum, l) l2i visited
    | HS.member l visited  = accum
    | not (M.member l l2i) = accum
    | otherwise            = accumulateBeforeLoop nextI l2i newVisited
        where
            nextI = evalInstruction (l2i M.! l) (accum,l)
            newVisited = HS.insert l visited
