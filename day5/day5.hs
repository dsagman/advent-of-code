module Main where
 
import Data.List ( transpose ) 
import Data.List.Split ( splitPlaces ) 
import Text.Regex.TDFA
    ( (=~), AllTextMatches(getAllTextMatches), RegexContext, Regex )

main :: IO ()
main = do
        dataFile <- readFile "day5/input.txt" 
        -- dataFile <- readFile "day5/test.txt" 
        let boxes = map (splitPlaces (repeat 4)) [x | x <- lines dataFile, '[' `elem` x]
        let cranes = map (\x -> read x :: Int) $ concatMap (splitPlaces (repeat 4)) [x | x <- lines dataFile, '[' `notElem` x && 'm' `notElem` x && '1' `elem` x]
        let moves = map (map (\x -> read x :: Int) . getMoves) [x | x <- lines dataFile, 'm' `elem` x]

        let stacks = zip cranes [map (take 3)$ filter ('[' `elem` ) $ transpose boxes !! (p-1) | p <- cranes]

    
        let answer1 = concatMap (take 1 . drop 1 . head . snd) $ doMoves moves stacks
        let answer2 = concatMap (take 1 . drop 1 . head . snd) $ doMoves' moves stacks

        putStr "Part 1 answer: " 
        print answer1

        
        putStr "Part 2 answer: " 
        print answer2
        
getMoves :: RegexContext Regex source1 (AllTextMatches [] String) => source1 -> [String]
getMoves x = getAllTextMatches (x =~ "[0-9]+") :: [String]

doMoves :: [[Int]] -> [(Int, [String])] -> [(Int, [String])]
doMoves [] stacks = stacks
doMoves (move:moves) stacks = doMoves moves (doMove move stacks)

doMove :: [Int] -> [(Int,[String])] -> [(Int,[String])]
doMove move stacks = [go move s| s <- stacks]
        where go m s
                | idx == from =  (idx, drop amt (snd (stacks !! (idx - 1))))
                | idx == head(to) = (idx, (reverse . take amt )(snd (stacks !! (from - 1))) ++ snd (stacks !! (idx - 1)))  
                | otherwise = s 
                where (amt:from:to) = m
                      idx = fst s
               
doMoves' :: [[Int]] -> [(Int, [String])] -> [(Int, [String])]
doMoves' [] stacks = stacks
doMoves' (move:moves) stacks = doMoves' moves (doMove' move stacks)

doMove' :: [Int] -> [(Int,[String])] -> [(Int,[String])]
doMove' move stacks = [go move s| s <- stacks]
        where go m s
                | idx == from =  (idx, drop amt (snd (stacks !! (idx - 1))))
                | idx == head(to) = (idx, (take amt )(snd (stacks !! (from - 1))) ++ snd (stacks !! (idx - 1)))  
                | otherwise = s 
                where (amt:from:to) = m
                      idx = fst s
                       
                 
                  
        