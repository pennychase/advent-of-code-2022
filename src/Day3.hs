{-# LANGUAGE OverloadedStrings #-}

module Day3 where

import Data.List.Split ( chunksOf )
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

priority :: Char -> Int
priority c
    | c >= 'a' && c <= 'z' = (fromEnum c - fromEnum 'a') + 1
    | c >= 'A' && c <= 'Z' = (fromEnum c - fromEnum 'A') + 27
    | otherwise = 0

toSet :: T.Text -> S.Set Char
toSet text = S.fromList . T.unpack $ text

process1 :: [T.Text] -> Int
process1 rucksacks =
    sum $ map processRucksack rucksacks
    where
        processRucksack :: T.Text -> Int
        processRucksack rucksack =
            priority item
            where
                (compart1, compart2) = T.splitAt (T.length rucksack `div` 2) rucksack
                compart1' = toSet compart1
                compart2' = toSet compart2
                item = S.elemAt 0 $ S.intersection compart1' compart2'

process2 :: [T.Text] -> Int
process2 rucksacks =
    sum $ processGroups rucksacks
    where
        processGroups :: [T.Text] -> [Int]
        processGroups rucksacks = 
            map processGroup groups
            where
                groups = map (map toSet) (chunksOf 3 rucksacks)

        processGroup :: [S.Set Char] -> Int
        processGroup group =
            priority $ S.elemAt 0 $ foldr S.intersection (head group) group


main :: IO ()
main = do
    contents <- TIO.readFile "../data/day3.txt"
    let rucksacks = T.lines contents
    let result1 = process1 rucksacks
    putStrLn $ "Part 1: " <> show result1
    let result2 = process2 rucksacks
    putStrLn $ "Part 2: " <> show result2

-- Test Data
test :: [T.Text]
test =
    [   "vJrwpWtwJgWrhcsFMMfFFhFp",
        "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL",
        "PmmdzqPrVvPwwTWBwg",
        "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn",
        "ttgJtRGJQctTZtZT",
        "CrZsJsPPZsGzwwsLwLmpwMDw"
    ]