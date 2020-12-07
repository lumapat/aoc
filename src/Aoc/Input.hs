module Aoc.Input
    ( AocInput (..)
    , Password (..)
    , parseAocInput
    ) where

import qualified Data.Text as T
import Data.Text.Lazy
import Data.Attoparsec.Text.Lazy as L

data Password = Password (Int, Int) Char String
                    deriving (Eq, Show)

-- TODO: Change invalid to something else?
data AocInput = Aoc20D1 [Int]
              | Aoc20D2 [Password]
              | Aoc20D3 [String]
              | Invalid

justInts :: Parser [Int]
justInts = L.many1' numbers where
    numbers = do
        d <- L.decimal
        L.endOfLine
        return $ d

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

textPerLine :: Parser String
textPerLine = do t <- L.takeTill (L.isEndOfLine)
                 L.endOfLine
                 return $ (T.unpack t)


aocParser :: Int -> Int -> Parser AocInput
aocParser 2020 1 = do ints <- justInts
                      return $ Aoc20D1 ints

aocParser 2020 2 = do passwords <- passwordParser
                      return $ Aoc20D2 passwords

aocParser 2020 3 = do ls <- L.many1' textPerLine
                      return $ Aoc20D3 ls

aocParser _ _ = return $ Invalid

parseAocInput :: Int -> Int -> String -> Either String AocInput
parseAocInput year day fileContents = eitherResult $ parse (aocParser year day) (pack fileContents)
