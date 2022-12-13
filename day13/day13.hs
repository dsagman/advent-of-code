module Main where

import Data.Char 
import Data.List 
import Data.List.Split 

main :: IO ()
main = do
        -- dataFile <- readFile "day13/input.txt"
        dataFile <- readFile "day13/test.txt"
        let packets = (filter (not . null) . lines) dataFile
        
        print packets


        let answer1 = "no"
        let answer2 ="dice"
        putStr "Part 1 answer: " 
        print answer1

        putStrLn "Part 2 answer: " 
        print answer2