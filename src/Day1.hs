{-# LANGUAGE OverloadedStrings #-}

module Day1 where

import Data.Ord ( comparing, Down(..) )
import Data.List ( sortBy )
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

readInt :: String -> Int
readInt = read 

sortElves :: T.Text -> [Int]
sortElves input = 
    sortBy (comparing Down) $ map processEach elves
    where
        elves = map (T.splitOn "\n") $ T.splitOn "\n\n" input
        processEach xs = foldr (\x y -> (readInt . T.unpack) x + y) 0 xs


main :: IO ()
main = do
    contents <- TIO.readFile "./data/day1.txt"
    let contents' = T.strip contents
    putStrLn $ "Part 1: " <> show (head (sortElves contents'))
    putStrLn $ "Part 2: " <> show ((sum . take 3) (sortElves contents'))

     
-- Test Data
test :: T.Text
test = "4000\n\n5000\n6000\n\n7000\n8000\n9000"