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

-- import Data.Array ( (!), (//), Array, array)
-- import qualified Data.Array as A
import Data.Array.Unboxed ( (!), (//), UArray, Array, array, indices, bounds, elems)
-- import qualified Data.Array as A


type XY = (Int, Int)
type Grid = UArray XY Bool
type Nbors = Array XY [XY]

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

nborsArray :: Grid -> Nbors 
nborsArray arr = array ((0,0),(size, size)) $
                 zip (indices arr) [nbors size xy | xy <- indices arr]
    where size = (snd . snd . bounds) arr

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
live :: XY -> Grid -> Nbors -> Bool
live xy@(x,y) arr nArr
    | s && (n == 2 || n == 3) = True
    | not s && n == 3         = True
    | otherwise               = False
    where s = arr ! xy
          n = numOn [arr ! nb | nb <- nArr ! xy]

live2 :: XY -> Grid -> Nbors -> Bool
live2 xy@(x,y) arr nArr
    | x == 1 && y == 1               = True
    | x == (size-1) && y == (size-1) = True
    | x == 1 && y == (size-1)        = True
    | x == (size-1) && y == 1        = True
    | s && (n == 2 || n == 3) = True
    | not s && n == 3         = True
    | otherwise               = False
    where s = arr ! xy
          n = numOn [arr ! nb | nb <- nArr ! xy]
          size = (snd . snd . bounds) arr

update (f,nArr) arr = arr // [ (xy, f xy arr nArr) | xy <- indices arr ]

main :: IO ()
main = do
    -- input <- readFile "./2015/day18/test"
    -- input <- readFile "./2015/day18/test2"
    input <- readFile "./2015/day18/day.txt"
    let size = 2 + minimum (map length (lines input))
    let lights = parseLights size (addBorder size input)
    print "Answer Part 1:"
    let lightNbors = nborsArray lights
    let life = iterate (update (live,lightNbors)) lights 
    -- mapM_ (displayGrid (size-1)) $ take 10 life
    print $ numOn . elems $ (life !! 100)

    print "Answer Part 2:"
    let life2 = iterate (update (live2,lightNbors)) lights
    print $ numOn . elems $ (life2 !! 100)


displayGrid :: Int -> Grid -> IO ()
displayGrid size arr = do
    putStrLn ""
    mapM_ putStrLn [ row y | y <- [0..size] ]
    putStrLn ""
  where
    row y = [ boolToChar (arr ! (x, y)) | x <- [0..size] ]


