module Main where
 
import Data.List ( intersect ) 
import Data.List.Split ( splitOneOf ) 

main :: IO ()
main = do
        dataFile <- readFile "day4/input.txt" 
        -- dataFile <- readFile "day4/test.txt" 
        let assigns1 = (filter (==True) . map (interpretAssigns1 . parseAssigns) . lines) dataFile

        putStr "Part 1 answer: " 
        print $ length assigns1

        let assigns2 = (filter (==True) . map (interpretAssigns2 . parseAssigns) . lines) dataFile
        
        putStr "Part 2 answer: " 
        print $ length assigns2

parseAssigns :: [Char] -> [Int]
parseAssigns = (map (\x -> read x :: Int) . splitOneOf "-,")

interpretAssigns1 :: (Enum a, Eq a) => [a] -> Bool
interpretAssigns1 [a,b,x,y] = (as == ins) || (xs == ins)
    where as = [a..b]
          xs = [x..y]
          ins = intersect as xs

interpretAssigns2 :: (Enum a, Eq a) => [a] -> Bool
interpretAssigns2 [a,b,x,y] = (not . null) ins
    where as = [a..b]
          xs = [x..y]
          ins = intersect as xs