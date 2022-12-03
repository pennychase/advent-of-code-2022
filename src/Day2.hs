{-# LANGUAGE OverloadedStrings #-}

module Day2 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Shape = Rock | Paper | Scissors
    deriving (Show, Eq, Enum)

instance Ord Shape where
    compare s1 s2 
        | s1 == s2 = EQ
        | s1 == Rock && s2 == Scissors = LT
        | s1 == Rock && s2 == Paper = GT
        | s1 == Scissors && s2 == Rock = GT
        | s1 == Scissors && s2 == Paper = LT
        | s1 == Paper && s2 == Rock = LT
        | s1 == Paper && s2 == Scissors = GT

outcome :: Shape -> Shape -> Int
outcome s1 s2 =
    case (compare s1 s2) of
        LT -> 0
        EQ -> 3
        GT -> 6

score :: Shape -> Shape -> Int
score player1 player2 =
    outcome player1 player2 + shape player2
    where
        shape player = 1 + fromEnum player

letterToShape :: T.Text -> Shape
letterToShape s =
    case s of
        "A" -> Rock
        "B" -> Paper
        "C" -> Scissors
        "X" -> Rock
        "Y" -> Paper
        "Z" -> Scissors

playRound :: T.Text -> Int
playRound s =
    score player1 player2
    where
        [player1, player2] = map letterToShape $ T.words s

playTornament :: [T.Text] -> Int
playTornament rounds = sum $ map playRound rounds


main :: IO ()
main = do
    contents <- TIO.readFile "../data/day2.txt"
    let rounds = T.lines contents
    let result = playTornament rounds
    print result




