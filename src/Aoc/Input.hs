module Aoc.Input
    ( AocInput (..)
    , parseAocInput
    ) where

import Data.Text.Lazy
import Data.Attoparsec.Text.Lazy as L

-- TODO: Change invalid to something else?
data AocInput = Aoc20D1P1 [Int]
                | Invalid

justInts :: Parser [Int]
justInts = L.many' numbers where
    numbers = do
        d <- L.decimal
        L.endOfLine
        return $ d

aocParser :: Int -> Int -> Parser AocInput
aocParser 2020 1 = do ints <- justInts
                      return $ Aoc20D1P1 ints

aocParser _ _ = return $ Invalid

parseAocInput :: Int -> Int -> String -> Either String AocInput
parseAocInput year day fileContents = eitherResult $ parse (aocParser year day) (pack fileContents)
