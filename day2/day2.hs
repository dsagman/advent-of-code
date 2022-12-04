module Main where

main :: IO ()
main = do
        nums <- readFile "day2/input.txt"
        let games1 = [[head x, (strategy1 . last) x] | x <- lines nums ]
        let myScores1 = zipWith (+) (map evalScore games1) (map evalBonus games1)

        putStr "Part 1 answer: " 
        print $ sum myScores1

        let games2 = [[head x, strategy2 x (last x)] | x <- lines nums ]
        let myScores2 = zipWith (+) (map evalScore games2) (map evalBonus games2)

        putStr "Part 2 answer: " 
        print $ sum myScores2

evalScore :: Num a => String -> a
evalScore x
    | x `elem` wins     = 6
    | head x == last x  = 3
    | otherwise         = 0

wins :: [String]
wins = ["AB", "BC", "CA"]

strategy1 :: Char -> Char
strategy1 s =
    case s of
        'X' -> 'A'
        'Y' -> 'B'
        'Z' -> 'C'

-- X means you need to lose, 
-- Y means you need to end the round in a draw, 
-- and Z means you need to win.

strategy2 :: [Char] -> Char -> Char
strategy2 g s =
    case s of
        'X' -> (head . head . filter (\x -> f == last x)) wins
        'Y' -> f
        'Z' -> (last . head . filter (\x -> f == head x)) wins
        where f = head g


evalBonus :: Num a => [Char] -> a
evalBonus x = 
    case last x of
        'A' -> 1
        'B' -> 2
        'C' -> 3


 
