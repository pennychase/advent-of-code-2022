module Day8 where

import Data.Char
import Data.List (transpose)


isVisible :: [Int] -> Int -> Bool
isVisible lis n
    | null front = True   -- front border
    | null back = True    -- back border
    | otherwise = lowerThan (head back) front || lowerThan (head back) (tail back)  -- interior elements 
    where
        (front, back) = splitAt n lis
        lowerThan x lis' = all (\y -> x > y) lis'

scenicScore :: [Int] -> Int -> Int
scenicScore lis n
    | null front = 0   -- front border
    | null back = 0    -- back border
    | otherwise = viewUntil (head back) (reverse front) * viewUntil (head back) (tail back)  -- interior elements 
    where
        (front, back) = splitAt n lis
        viewUntil :: Int -> [Int] -> Int
        viewUntil x ys
            | null ys = 0
            | (head ys) >= x = 1
            | otherwise = 1 + viewUntil x (tail ys)


-- Since we're only looking at the rows and columns for a given element, didn't need to index
-- (although it might be faster if using vectors). Created a list of list for row-wise and transposed
-- it for column-wise.
-- To get the final answer we concat the rows and concat the transpose columns, so now the lists
-- represent each cell in the same order. We use zipWidth to combine the row-wise and column-wise"scores" 
-- and use the appropriate functions to summarize for the result.

part1 input =
    length $ filter (== True) $ zipWith (||) (concat rs) (concat (transpose cs))
    where
        n = length input - 1
        rs = map (\x -> map (isVisible x) [0 .. n]) input
        cs = map (\x -> map (isVisible x) [0 .. n]) (transpose input)

part2 input =
    maximum $ zipWith (*) (concat rs) (concat (transpose cs))
    where
        n = length input - 1
        rs = map (\x -> map (scenicScore x) [0 .. n]) input
        cs = map (\x -> map (scenicScore x) [0 .. n]) (transpose input)

main :: IO ()
main = do
    contents <- readFile "./data/day8.txt"
    let input = map (map digitToInt) $ lines contents
    putStrLn $ "Part 1: " <> show (part1 input)
    putStrLn $ "Part 2: " <> show (part2 input)

-- Test data
test = "30373\n25512\n65332\n33549\n35390\n"
