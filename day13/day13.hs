module Main where

import Data.Char 
import Data.List 
import Data.List.Split 
import Text.Regex.TDFA
    ( (=~), AllTextMatches(getAllTextMatches), RegexContext, Regex )

main :: IO ()
main = do
        -- dataFile <- readFile "day13/input.txt"
        dataFile <- readFile "day13/test.txt"
        let packets = (map (filter (not . null)) . 
                        filter (not . null) . 
                        map (split $ oneOf (",[]"))  . 
                        lines) dataFile
        print $ head packets


        let answer1 = "no"
        let answer2 ="dice"
        putStr "Part 1 answer: " 
        print answer1

        putStrLn "Part 2 answer: " 
        print answer2

data P =  P Int | String

scanner (x:y:xs) = [(x,y)] ++ scanner xs

parser (x:xs) 
        | length x == 1 = x
        | x == "[" = [parser xs]
        | otherwise = xs

-- findInt :: [Char] -> P
-- findInt x 
--         | head x `elem` ['0'..'9'] = read x :: Int
--         | otherwise = x