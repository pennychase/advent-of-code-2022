{-# LANGUAGE OverloadedStrings #-}

module Day4 where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char

import Parser

data Ranges = Ranges (Int,Int) (Int,Int)
    deriving Show

contains :: Ranges -> Bool
contains (Ranges r1 r2) =
    r1 `containedIn` r2 || r2 `containedIn` r1
    where 
        (s1, e1) `containedIn` (s2, e2) = s1 >= s2 && e1 <= e2 

overlaps :: Ranges -> Bool
overlaps (Ranges r1 r2) =
    r1 `leftOverlap` r2 || r2 `leftOverlap` r1
    where
        (s1, e1) `leftOverlap` (s2, e2) = s1 <= s2 && e1 >= s2

solve :: (Ranges -> Bool) -> [Ranges] -> Int
solve f rs = sum $ map (fromEnum . f) rs

-- Parsing
parseRanges :: Parser Ranges
parseRanges = do
    range1 <- pairParser '-'
    char ','
    range2 <- pairParser '-'
    pure $ Ranges range1 range2

parseSections :: Parser [Ranges]
parseSections = sepEndBy1 parseRanges eol

main :: IO ()
main = do
    input <- readInput "./data/day4.txt" parseSections
    let result1 = solve contains input
    putStrLn $ "Part 1: " <> show result1
    let result2 = solve overlaps input
    putStrLn $ "Part 2: " <> show result2
    
    


-- Test data
test :: String
test = "2-4,6-8\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"