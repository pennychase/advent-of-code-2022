module Day6 where

import Data.Set (Set)
import qualified Data.Set as S

-- Finds the marker for the first times there are num unique characters in the stream. Creates a set
-- of num chacaters from the beginning of the stream and if the size of the set is num, the current index + num 
-- is the marker.
findUnique :: Int -> String -> Int 
findUnique num stream =
    findUnique' stream 0
    where
        findUnique' str cur =
            if S.size uniqueChars == num
                then cur + num
                else findUnique' (tail str) (cur + 1)
            where
                uniqueChars = S.fromList (take num str)

main :: IO ()
main = do
    contents <- readFile "./data/day6.txt"
    putStrLn $ "Part 1: " <> show (findUnique 4 contents)   -- marker for start of packet (4 chars)
    putStrLn $ "Part 2: " <> show (findUnique 14 contents)  -- marker for start of message (14 chars)