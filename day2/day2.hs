module Main where

main :: IO ()
main = do
        nums <- readFile "day2/input.txt"
        let games = [[head x, (strategy . last) x] | x <- lines nums ]
        let myScores = zipWith (+) (map evalScore games) (map evalBonus games)

        putStr "Part 1 answer: " 
        print $ sum myScores

        -- putStr "Part 2 answer: " 
        -- print $ (sum . take 3 . reverse . sort) food

evalScore :: Num a => String -> a
evalScore x
    | x `elem` wins     = 6
    | head x == last x  = 3
    | otherwise         = 0
    where wins = ["AB", "BC", "CA"]

strategy :: Char -> Char
strategy x =
    case x of
        'X' -> 'A'
        'Y' -> 'B'
        'Z' -> 'C'

evalBonus :: Num a => [Char] -> a
evalBonus x = 
    case last x of
        'A' -> 1
        'B' -> 2
        'C' -> 3


 
