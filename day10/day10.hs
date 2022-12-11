module Main where
import Data.List.Split ( chunksOf )

main :: IO ()
main = do
        dataFile <- readFile "day10/input.txt" 
        -- dataFile <- readFile "day10/test.txt" 
        let commands = lines dataFile

        let p = parse commands [1]
        let sampleIndx = [20,60..(2 * length p)]
        let signals = [(s) * (p !! (s-1)) | s <- sampleIndx, s <= length p]
        let answer1 = sum signals

        let crt = zip (init p)  [0..] 
        let answer2 = chunksOf 40 $ map draw crt

        putStr "Part 1 answer: " 
        print answer1

        putStrLn "Part 2 answer: " 
        mapM_ print $ answer2


draw :: Integral a => (a, a) -> Char
draw (signal, clock) 
    | (signal - (clock `mod` 40)) `elem` [1,0,-1] = '#'
    | otherwise = '.' 


parse :: [String] -> [Int] -> [Int]
parse [] s = s
parse (c:cs) curState
    | c == "noop" =  parse cs noop
    | otherwise =  parse cs addx
        where 
            noop = curState ++ [s]
            addx = curState ++ [s, s+n]
            s = last curState
            n = read (drop 5 c) :: Int