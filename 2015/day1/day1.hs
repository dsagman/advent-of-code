module Main where

import Control.Monad.State

    -- (()) and ()() both result in floor 0.
    -- ((( and (()(()( both result in floor 3.
    -- ))((((( also results in floor 3.
    -- ()) and ))( both result in floor -1 (the first basement level).
    -- ))) and )())()) both result in floor -3.

countFloor :: String -> State Int ()
countFloor [] = return ()
countFloor (x:xs) = do
    currentFloor <- get
    put (currentFloor + parseParen x)
    countFloor xs

parseParen x =
    case x of
        '(' -> 1
        ')' -> -1
        _   -> 0

findBasement :: String -> State (Int, Int) ()
findBasement [] = return ()
findBasement (x:xs) = do
    (currentFloor, steps) <- get
    let newFloor = currentFloor + parseParen x
    let newStep = steps + 1
    put (currentFloor + parseParen x, steps + 1)
    if newFloor == (-1)
        then return ()
        else findBasement xs


main :: IO ()
main = do
    input <- readFile "./2015/day1/day.txt"
    print "Part 1 solution 2 ways:"
    print $ execState (countFloor input) 0
    let parsed = [parseParen x | x <- input]
    print $ sum parsed

    print "Part 2 solution 2 ways:"
    print $ (snd . head . filter ((==(-1)) . fst)) $ zip (scanl1 (+) parsed) [1..]
    print $ snd $ execState (findBasement input) (0,0)
    