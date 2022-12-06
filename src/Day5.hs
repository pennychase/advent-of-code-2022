module Day5 where

import Data.List (transpose, foldl', splitAt)
import Data.Map (Map)
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char

import Parser

type Number = Int
type From = Int
type To = Int 

data Move = Move Number From To
    deriving Show

move :: Move -> Map Int String -> Map Int String
move (Move num from to) m = undefined
{-
    M.insert from restFrom (M.insert to newToStack m)
    where
        fromStack = M.lookup from m
        toStack = M.lookup to m
        (poppedFrom, restFrom) = splitAt num fromStack -- pop fromStack
        newToStack = foldl' (\x y -> y:x) toStack poppedFrom -- push onto to stack

-}

makeStacks :: [String] -> Map Int String
makeStacks strs = 
    stacks
    where
        n = length . head $ strs
        stacks = M.fromList $ zip [1 .. n] (map (filter (' ' /=)) (transpose strs))

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