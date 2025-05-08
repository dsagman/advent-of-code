-- Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.

-- To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.

-- For example:

--     turn on 0,0 through 999,999 would turn on (or leave on) every light.
--     toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
--     turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.

-- After following the instructions, how many lights are lit?

--- Part Two ---

-- You just finish implementing your winning light pattern when you realize you mistranslated Santa's message from Ancient Nordic Elvish.

-- The light grid you bought actually has individual brightness controls; each light can have a brightness of zero or more. The lights all start at zero.

-- The phrase turn on actually means that you should increase the brightness of those lights by 1.

-- The phrase turn off actually means that you should decrease the brightness of those lights by 1, to a minimum of zero.

-- The phrase toggle actually means that you should increase the brightness of those lights by 2.

-- What is the total brightness of all lights combined after following Santa's instructions?

-- For example:

--     turn on 0,0 through 0,0 would increase the total brightness by 1.
--     toggle 0,0 through 999,999 would increase the total brightness by 2000000.


import Data.Array ( (!), (//), array, elems, Array)
import Data.List (foldl')

import Text.Parsec (char, digit, newline, space, string, (<|>), many, many1, parse, try )      
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)

type Cmd = (String, XY, XY)
type XY = (Int, Int)
type Grid = Array (Int, Int) Int

cmdParser :: Parser (String, XY, XY)
cmdParser = do
    cmd <- try (string "turn on")
       <|> try (string "turn off")
       <|> string "toggle"
    space
    x1 <- read <$> many1 digit
    char ','
    y1 <- read <$> many1 digit
    string " through "
    x2 <- read <$> many1 digit
    char ','
    y2 <- read <$> many1 digit
    return (cmd, (x1, y1), (x2, y2))

inputParser :: Parser [Cmd]
inputParser =  do
    many $ try (cmdParser <* newline)


size :: Int
size = 999

evalPart1 :: Cmd -> Grid -> Grid
evalPart1 (inst, xy1, xy2) a = a // [((x,y),f x y) | x <- [x1..x2], y <- [y1..y2]]
    where
        (x1,y1) = xy1
        (x2,y2) = xy2 
        f x y = case inst of
            "turn off" -> 0
            "turn on"  -> 1
            "toggle"   -> 1-a!(x,y)

evalPart2 :: Cmd -> Grid -> Grid
evalPart2 (inst, xy1, xy2) a = a // [((x,y),f x y) | x <- [x1..x2], y <- [y1..y2]]
    where
        (x1,y1) = xy1
        (x2,y2) = xy2 
        f x y = case inst of
            "turn off" -> max 0 (a!(x,y)-1)
            "turn on"  -> a!(x,y)+1
            "toggle"   -> a!(x,y)+2

lights :: Grid
lights= array ((0,0),(size,size)) [((x,y),0) | x <- [0..size], y <-[0..size]]

numLit :: Grid -> Int
numLit =  sum . elems

main :: IO ()
main = do
    input <- readFile "./2015/day6/day.txt"
    dataSet <- case parse inputParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x
    
    let lightsPart1 = foldl' (flip evalPart1) lights dataSet
    print "Answer Part 1:"
    print $ numLit lightsPart1
    let lightsPart2 = foldl' (flip evalPart2) lights dataSet
    print "Answer Part 2:"
    print $ numLit lightsPart2


displayGrid :: Grid -> IO ()
displayGrid arr = mapM_ putStrLn [ row y | y <- [0..size] ]
  where
    row y = [ display (arr ! (x, y)) | x <- [0..size] ]
    display 0 = '.'  -- for example, 0 = off
    display _ = '#'  -- non-zero = on

