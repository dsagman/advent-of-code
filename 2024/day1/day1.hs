module Main where

import Data.List ( sort )
import Data.Bifunctor ( Bifunctor(bimap) )



main :: IO ()
main = do
        inputLines <- readFile "2024/day1/day.txt"
        let nums = [(read (take 5 xs) :: Int, read (drop 8 xs) :: Int)| xs <- lines inputLines]
        -- inputLines <- readFile "2024/day1/test.txt"
        -- let nums = [(read (take 1 xs) :: Int, read (drop 4 xs) :: Int)| xs <- lines inputLines]

        -- let sortedNums = zip (sort $ map fst nums) (sort $ map snd nums)
        let sortedNums = map abs $ uncurry (zipWith (-)) $ bimap sort sort (unzip nums)
        -- let a = map fst nums
        -- let b = map snd nums
        let (a, b) = unzip nums
        let countNums = zipWith (*) a (map (\x -> length $ filter (==x) b) a)

        putStr "Part 1 answer: "
        print $ sum sortedNums

        putStr "Part 2 answer: " 
        print $ sum countNums 

