module Aoc.Input
    ( AocInput (..)
    , Password (..)
    , parseAocInput
    ) where

import Control.Applicative ((<|>), (*>), optional)
import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Attoparsec.Text.Lazy as L

-- Q1
justInts :: Parser [Int]
justInts = L.many1' numbers where
    numbers = do
        d <- L.decimal
        L.endOfLine
        return $ d


-- Q2
data Password = Password (Int, Int) Char String
                    deriving (Eq, Show)

passwordParser :: Parser [Password]
passwordParser = L.many1' passwords where
    passwords = do
        lowerBound <- L.decimal
        char '-'
        upperBound <- L.decimal
        space
        c <- anyChar
        char ':'
        space
        p <- L.takeTill (L.isEndOfLine)
        L.endOfLine
        return $ Password (lowerBound, upperBound) c (T.unpack p)

-- Q3
textPerLine :: Parser String
textPerLine = do t <- L.takeTill (L.isEndOfLine)
                 L.endOfLine
                 return $ (T.unpack t)

-- Q4
passportBlock :: Parser [(String, String)]
passportBlock = do fieldLines <- L.manyTill passportField (L.endOfLine <|> L.endOfInput)
                   return $ fieldLines

passportField :: Parser (String, String)
passportField = do f <- L.takeTill (==':')
                   char ':'
                   v <- L.takeTill isSpace
                   L.space
                   return $ (T.unpack f, T.unpack v)

-- Q6
newlineBlock :: Parser a -> Parser [a]
newlineBlock p = do block <- L.manyTill p (L.endOfLine <|> L.endOfInput)
                    return $ block

textBlock = newlineBlock textPerLine

-- Q7
bagDescription :: Parser String
bagDescription = do variant <- L.takeTill isSpace
                    L.space
                    color <- L.takeTill isSpace
                    L.space
                    let singularBag = L.string $ T.pack "bag"
                    let pluralBags  = L.string $ T.pack "bags"
                    pluralBags <|> singularBag
                    return $ (T.unpack variant) <> " " <> (T.unpack color)

bagQuantity :: Parser (String, Int)
bagQuantity = do n <- L.decimal
                 L.space
                 color <- bagDescription
                 return $ (color, n)

bagWithNoCapacity :: Parser [(String, Int)]
bagWithNoCapacity = do L.string $ T.pack "no other bags"
                       return $ []

bagWithCapacity :: Parser [(String, Int)]
bagWithCapacity = bagQuantity `L.sepBy1` (L.string $ T.pack ", ")


bagRule :: Parser (String, [(String, Int)])
bagRule = do color <- bagDescription
             L.string $ T.pack " contain "
             quantities <- bagWithNoCapacity <|> bagWithCapacity
             L.char '.'
             return $ (color, quantities)

-- TODO: Change invalid to something else?
data AocInput = Aoc20D1 [Int]
              | Aoc20D2 [Password]
              | Aoc20D3 [String]
              | Aoc20D4 [[(String, String)]]
              | Aoc20D5 [String]
              | Aoc20D6 [[String]]
              | Aoc20D7 [(String, [(String, Int)])]
              | Invalid

aocParser :: Int -> Int -> Parser AocInput
aocParser 2020 1 = do ints <- justInts
                      return $ Aoc20D1 ints

aocParser 2020 2 = do passwords <- passwordParser
                      return $ Aoc20D2 passwords

aocParser 2020 3 = do ls <- L.many1' textPerLine
                      return $ Aoc20D3 ls

aocParser 2020 4 = do passports <- L.manyTill passportBlock L.endOfInput
                      return $ Aoc20D4 $ filter (not . null) passports

aocParser 2020 5 = do ls <- L.many1' textPerLine
                      return $ Aoc20D5 ls

aocParser 2020 6 = do ls <- textBlock `L.manyTill` L.endOfInput
                      return $ Aoc20D6 ls

aocParser 2020 7 = do rules <- bagRule `L.sepBy1` L.endOfLine
                      return $ Aoc20D7 rules

aocParser _ _ = return $ Invalid

parseAocInput :: Int -> Int -> String -> Either String AocInput
parseAocInput year day fileContents = eitherResult $ parse (aocParser year day) (LT.pack fileContents)
