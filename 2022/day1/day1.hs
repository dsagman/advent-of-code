module Main where

import Data.List.Split ( splitOn )
import Data.List ( sort )

main :: IO ()
main = do
        nums <- readFile "day1/input1.txt"
        let food = (map sum . splitOn [0] . map numOrNull . lines) nums 
       
        putStr "Part 1 answer: " 
        print $ maximum food

        putStr "Part 2 answer: " 
        print $ (sum . take 3 . reverse . sort) food


numOrNull :: [Char] -> Int
numOrNull x 
    | null x = 0
    | otherwise = read x :: Int