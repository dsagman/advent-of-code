{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Map.Strict as Map
import Text.Megaparsec (Parsec, parse, takeWhileP, eof, errorBundlePretty, many)
import Data.Void ( Void )
import Data.Either (fromRight)
import Text.Megaparsec.Char ( asciiChar, newline )
import Data.List (transpose)
import Control.Monad.State ( modify, execState, State )

type Parser = Parsec Void String

type Grid = [[Char]]

gridParser :: Parser (Grid, String)
gridParser = do
    gridLines <- many $ takeWhileP (Just "gridLine") isGrid  <* newline
    commands <- many asciiChar
    eof
    return (gridLines, commands)

isAt :: Char -> Bool
isAt c = c == '@'

isBox :: Char -> Bool
isBox c = c == 'O'

isWallorSpaceorBox :: Char -> Bool
isWallorSpaceorBox c = c == '#' || c == '.' || c == 'O'

isGrid :: Char -> Bool
isGrid c = isAt c || isWallorSpaceorBox c

atLineParse :: Parser (String, String, String, String)
atLineParse = do
    before <- takeWhileP (Just "before") isWallorSpaceorBox
    at <- takeWhileP (Just "at") isAt
    boxes <- takeWhileP (Just "boxes") isBox
    after <- takeWhileP (Just "after") isWallorSpaceorBox
    return (before, at, boxes, after)

main :: IO ()
main = do
    let dataFile = "2024/day15/day.txt"
    input <- readFile dataFile
    let parsed = parse gridParser "" input
    case parsed of
        Left bundle -> do
            putStr $ errorBundlePretty bundle
            error "parse error"
        Right (grid, commands) -> do
            print "Parsed!"
    let (grid, commands) = fromRight ([""],[]) parsed
    mapM_ putStrLn grid
    let allMoves = foldl (\g c -> execState (moveGrid c) g) grid commands
    mapM_ putStrLn allMoves
    print $ scoreGrid allMoves
    print "Day 15"


scoreGrid :: Grid -> Int
scoreGrid grid = sum [x + (y * 100) | ((x, y), _) <- boxIdx]
        where
            boxIdx = concatMap (filter (\((_, _), c) -> c == 'O')) (zipIndexes grid)


moveGrid :: Char -> State Grid ()
moveGrid c  = modify $ case c of
    'v' -> transpose . map moveAt . transpose
    '^' -> transpose . map ((reverse . moveAt) . reverse) . transpose
    '<' -> map ((reverse . moveAt) . reverse)
    '>' -> map moveAt
    _ -> id

-- we can try to use this later to make more efficient
splitGrid :: Grid -> [Grid]
splitGrid grid = before : [head after] : [tail after]
        where (before, after) = break (\x -> '@' `elem` x) grid


moveAt :: [Char] -> [Char]
moveAt xs
    | '@' `notElem` xs  = xs
    | head after == '.' = before ++ "." ++ at ++ boxes ++ tail after
    | otherwise = xs
    where (before,at,boxes,after) = fromRight ("","","","") $ parse atLineParse "" xs

zipIndexes :: [[a]] -> [[((Int, Int), a)]]
zipIndexes = zipWith (\y xs -> zipWith (\x a -> ((x, y), a)) [0..] xs) [0..]

-- 

-- agrid :: [[Char]]
-- agrid = ["########",
--         "#..@OO.#",
--         "##..O..#",
--         "#...O..#",
--         "#.#.O..#",
--         "#...O..#",
--         "#......#",
--         "########"]

-- a :: String
-- a = "#..@OO.#"
-- b :: String
-- b = "##..@..#"
-- c = "##@.O..#"


-- printGridMap :: Map.Map (Int, Int) Char -> IO ()
-- printGridMap gridMap = do
--     let (w, h) = fst $ Map.elemAt (Map.size gridMap - 1) gridMap
--     let grid = [[Map.findWithDefault ' ' (x, y) gridMap | x <- [0..w]] | y <- [0..h]]
--     mapM_ print grid

-- moveGrid :: Char -> [[Char]] -> [[Char]]
-- moveGrid c  = case c of
--     'v' -> transpose . map moveAt . transpose 
--     '^' -> transpose .  map reverse . map moveAt . map reverse  . transpose 
--     '<' -> map reverse . map moveAt . map reverse
--     '>' -> map moveAt

    -- let right = puz
    -- let left  = map reverse puz
    -- let down  = transpose puz
    -- let up    = map reverse $ transpose puz



