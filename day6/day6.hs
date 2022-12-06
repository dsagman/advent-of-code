module Main where
 
import Data.List ( nub ) 

main :: IO ()
main = do
        dataFile <- readFile "day6/input.txt" 
    
        let answer1 = (length . head . scanM 4) ([],dataFile) 
        let answer2 = (length . head . scanM 14) ([],dataFile) 

        putStr "Part 1 answer: " 
        print answer1

        putStr "Part 2 answer: " 
        print answer2


scanM :: Eq a => Int -> ([a], [a]) -> [[a]]
scanM n (x,(y:ys)) 
    | (length x < n) || (nub scanList /= scanList) = scanM n (x ++ [y], ys) 
    | otherwise = [x, y:ys]
    where 
        scanList = (take n . reverse) x
