{-# LANGUAGE OverloadedStrings #-}

module Day5 where

import Data.List (transpose, foldl', splitAt)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Megaparsec
import Text.Megaparsec.Char

import Parser

type Number = Int
type From = Int
type To = Int 

data Move = Move Number From To
    deriving Show

data PushType = One | All

move :: PushType -> Move -> Map Int String -> Map Int String
move pt (Move num from to) m =
    M.insert from restFrom (M.insert to newToStack m)
    where
        (Just fromStack) = M.lookup from m
        (Just toStack) = M.lookup to m
        (poppedFrom, restFrom) = splitAt num fromStack -- pop fromStack
        newToStack = case pt of   -- push onto to stack
            One -> foldl' (\x y -> y:x) toStack poppedFrom 
            All -> poppedFrom <> toStack

allMoves :: PushType -> [Move] -> Map Int String -> Map Int String
allMoves _ [] stacks = stacks
allMoves pt (m:ms) stacks = allMoves pt ms (move pt m stacks)

part1 :: [Move] -> Map Int String -> String
part1 moves stacks = map head $ M.elems (allMoves One moves stacks)
    
part2 :: [Move] -> Map Int String -> String
part2 moves stacks = map head $ M.elems (allMoves All moves stacks)
    


-- Processing input
-- The input file is in two part: the stacks and the moves. To read it:
-- 1. split file into two parts 
-- 2. parse the first part to get stack input as a list of rows. Note we
--    delete the row of the stack numbers since we compute from the rows
--    (the numbers helped make clear that each row consisted of stack entries (3 chars) 
--    separated by spaces)
-- 3. convert the list of rows into a list of stacks with makeStacks
-- 3. parse the commands

makeStacks :: [String] -> Map Int String
makeStacks strs = 
    stacks
    where
        n = length . head $ strs
        stacks = M.fromList $ zip [1 .. n] (map (filter (' ' /=)) (transpose strs))

processInput :: String -> String -> IO (Map Int String, [Move])
processInput stacksStr movesStr = do
    stacks <- readInput' stacksStr parseRows
    moves <- readInput' movesStr parseMoves
    pure $ (makeStacks stacks, moves)

-- Parsing

-- Parsing the stacks

parseEmpty :: Parser Char
parseEmpty = do
    count 3 (char ' ')
    pure $ ' '

parseItem :: Parser Char
parseItem = do
    char '['
    c <- alphaNumChar 
    char ']'
    pure $ c

parseRow:: Parser String
parseRow = do
    (parseEmpty <|> parseItem) `sepBy` (char ' ')

parseRows :: Parser [String]
parseRows = do
    sepEndBy1 parseRow eol

-- Parsing the moves

parseMove :: Parser Move
parseMove = do
    string' "move "
    number <- intParser
    string' "from "
    from <- intParser
    string' "to "
    to <- intParser
    pure $ Move number from to

parseMoves :: Parser [Move]
parseMoves = do
    sepEndBy1 parseMove eol

main :: IO ()
main = do
    contents <- TIO.readFile "./data/day5.txt"
    let [str1, str2] = T.splitOn ("\n\n") contents
    let str1' = T.unlines $ init $ T.lines str1
    (stacks, moves) <- processInput (T.unpack str1') (T.unpack str2)
    putStrLn $ "Part 1: " <> part1 moves stacks
    putStrLn $ "Part 2: " <> part2 moves stacks
  


