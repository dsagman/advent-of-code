module Main where

import Data.Char ( ord, isLower )
import Data.List ( intersect, nub )
import Data.List.Split ( chunksOf )

main :: IO ()
main = do
        dataFile <- readFile "day3/input.txt"
        let sacks1 = (map splitInHalf . lines) dataFile
        let commonItem1 = map (head . nub . uncurry intersect) sacks1

        putStr "Part 1 answer: " 
        print $ (sum . map getPriority) commonItem1

        let sacks2 = (chunksOf 3 . lines) dataFile
        let commonItem2 = map (head . nub . intersect3 ) sacks2

        putStr "Part 2 answer: " 
        print $ (sum . map getPriority) commonItem2

getPriority :: Char -> Int
getPriority x =
    case isLower x of 
        True -> ord x - 96
        otherwise -> ord x - 38

splitInHalf :: [a] -> ([a], [a])
splitInHalf x = splitAt (length x `div` 2) x

intersect3 :: Eq a => [[a]] -> [a]
intersect3 [x,y,z] = intersect (intersect x y) z 