{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}

{-
For Part 1, I originally solved using the ordering on Shape (writing compare function).

For Part 2, I decided to redo the solution to handle both parts by defining a CyclicEnum
class and making Shape an instance (using DerivingAnyClass). In addition to using this to
detemine the outcome of a round, it also enables the creation of the rounds for part 2 
(since winning/losing/drawing can be stated in terms of the successor and predecessor functions).
-}

module Day2 where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

class (Eq a, Enum a, Bounded a) => CyclicEnum a where

    cpred :: a -> a
    cpred x
        | x == minBound = maxBound
        | otherwise = pred x

    csucc :: a -> a
    csucc x
        | x == maxBound = minBound
        | otherwise = succ x

data Shape = Rock | Paper | Scissors
    deriving (Show, Eq, Enum, Bounded, CyclicEnum)

outcome :: Shape -> Shape -> Int
outcome s1 s2
    | s1 == s2       = 3
    | csucc s1 == s2 = 6
    | cpred s1 == s2 = 0

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

letterToOutcomeShape :: T.Text -> T.Text -> Shape
letterToOutcomeShape p1 o =
    case o of
        "X" -> cpred (letterToShape p1)     -- need to play shape to lose
        "Y" -> letterToShape p1             -- need to play shhape to draw
        "Z" -> csucc (letterToShape p1)     -- need tp play shape to win

part1Round :: T.Text -> T.Text -> (Shape, Shape)
part1Round player1 player2 = (letterToShape player1, letterToShape player2)

part2Round :: T.Text -> T.Text -> (Shape, Shape)
part2Round player1 player2 = (letterToShape player1, letterToOutcomeShape player1 player2)

playRound :: (T.Text -> T.Text -> (Shape, Shape)) -> T.Text -> Int
playRound makeRound s =
    score player1' player2'
    where
        [player1, player2] = T.words s
        (player1', player2') = makeRound player1 player2

playTornament :: (T.Text -> T.Text -> (Shape, Shape)) -> [T.Text] -> Int
playTornament makeRound rounds = sum $ map (playRound makeRound) rounds


main :: IO ()
main = do
    contents <- TIO.readFile "../data/day2.txt"
    let rounds = T.lines contents
    let result1 = playTornament part1Round rounds
    let result2 = playTornament part2Round rounds
    print result1
    print result2





