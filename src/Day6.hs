module Day6 where

import Data.Set (Set)
import qualified Data.Set as S

findUnique :: Int -> String -> Int 
findUnique num stream =
    findUnique' stream 0
    where
        findUnique' str cnt =
            if S.size uniqueChars == num
                then cnt + num
                else findUnique' (tail str) (cnt + 1)
            where
                uniqueChars = S.fromList (take num str)

main :: IO ()
main = do
    contents <- readFile "./data/day6.txt"
    putStrLn $ "Part 1: " <> show (findUnique 4 contents)
    putStrLn $ "Part 2: " <> show (findUnique 14 contents)