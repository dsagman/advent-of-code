{-# LANGUAGE BangPatterns #-}
-- Start by setting your lights to the included initial configuration (your puzzle input). A # means "on", and a . means "off".

-- The state a light should have next is based on its current state (on or off) plus the number of neighbors that are on:

-- A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise.
-- A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.

-- All of the lights update simultaneously; they all consider the same current state before moving to the next.

-- Here's a few steps from an example configuration of another 6x6 grid:

-- Initial state:
-- .#.#.#
-- ...##.
-- #....#
-- ..#...
-- #.#..#
-- ####..

-- After 1 step:
-- ..##..
-- ..##.#
-- ...##.
-- ......
-- #.....
-- #.##..

-- After 2 steps:
-- ..###.
-- ......
-- ..###.
-- ......
-- .#....
-- .#....

-- After 3 steps:
-- ...#..
-- ......
-- ...#..
-- ..##..
-- ......
-- ......

-- After 4 steps:
-- ......
-- ......
-- ..##..
-- ..##..
-- ......
-- ......

-- After 4 steps, this example has four lights on.

-- In your grid of 100x100 lights, given your initial configuration, how many lights are on after 100 steps?

import Data.Array ( (!), (//), Array, listArray, array)
import qualified Data.Array as A
import Data.List

type XY = (Int, Int)
type Grid = Array XY Bool

type GridData = (Grid, Array XY [XY])

buildGridData :: Grid -> GridData
buildGridData arr = (arr, A.listArray (A.bounds arr) [nbors size xy | xy <- A.indices arr])
  where size = snd . snd . A.bounds $ arr



charToBool :: Char -> Bool
charToBool c = case c of
                '.' -> False
                '#' -> True

boolToChar :: Bool -> Char
boolToChar b = if b then '#' else '.'

parseLights :: Int -> [Char] -> Grid
parseLights size = array ((0,0),(size-1,size-1)) .
                zipWith (\ xy v -> ((xy `mod` size, xy `div` size), charToBool v)) [0..]

addBorder :: Int -> String -> [Char]
addBorder size xs = replicate size '.' ++
                    concatMap (\x -> '.' : x ++ ".") (lines xs) ++
                    replicate size '.'

nbors :: Int -> XY -> [XY]
nbors size (x,y)
    | x == 0    || y == 0    = []
    | x == size || y == size = []
    | otherwise = [(x+dx,y+dy) | dx<-[-1..1], dy<-[-1..1],
                        not (dx==0 && dy==0)]

numOn :: [Bool] -> Int
numOn = length . filter id

-- A light which is on stays on when 2 or 3 neighbors are on, and turns off otherwise.
-- A light which is off turns on if exactly 3 neighbors are on, and stays off otherwise.
-- live :: Int -> XY -> Grid -> Bool
-- live size xy arr
--     | s && (n == 2 || n == 3) = True
--     | not s && n == 3         = True
--     | otherwise               = False
--     where !s = arr ! xy
--           !n = numOn [arr ! nb | nb <- nbors size xy]
live' :: GridData -> XY -> Bool
live' (arr, nbArr) xy =
    let s = arr ! xy
        n = numOn [arr ! nb | nb <- nbArr ! xy]
    in (s && (n == 2 || n == 3)) || (not s && n == 3)

-- update :: Int -> Grid -> Grid
-- update size arr = arr // [ (xy, live size xy arr) | xy <- A.indices arr ]

update' :: GridData -> GridData
update' (arr, nbArr) =
    let arr' = arr // [(xy, live' (arr, nbArr) xy) | xy <- A.indices arr]
    in (arr', nbArr)

main :: IO ()
main = do
    -- input <- readFile "./2015/day18/test"
    input <- readFile "./2015/day18/day.txt"
    let size = 2 + minimum (map length (lines input))
    let lights = parseLights size (addBorder size input)
    -- displayGrid size lights
    -- let life = iterate (update (size-1)) lights
    -- mapM_ displayGrid size $ take 100 life
    -- print $ (length . filter id . A.elems) lights
    print "Answer Part 1:"
    -- print $ numOn . A.elems $ (life !! 100)
    let gridData = buildGridData lights
    let life = iterate update' gridData
    print $ numOn . A.elems . fst $ (life !! 100)


displayGrid :: Int -> Grid -> IO ()
displayGrid size arr = do
    putStrLn ""
    mapM_ putStrLn [ row y | y <- [0..size] ]
    putStrLn ""
  where
    row y = [ boolToChar (arr ! (x, y)) | x <- [0..size] ]

