-- Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.

-- To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.

-- For example:

--     turn on 0,0 through 999,999 would turn on (or leave on) every light.
--     toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
--     turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.

-- After following the instructions, how many lights are lit?

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.List (foldl')

import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)

type Cmd = (String, XY, XY)
type XY = (Int, Int)
type Grid = (Map (Int, Int) Int)
type Instruction = (String, XY, XY)

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

-- inputParser :: Parser [[Int]]
inputParser =  do
    many $ try (cmdParser <* newline)


size :: Int
size = 1000

keys :: XY -> XY -> [XY]
keys (x1,y1) (x2,y2) = [(x,y) | x <- [x1..x2], y <- [y1..y2]]

toggle :: XY -> XY -> Grid -> Grid
toggle xy1 xy2 grid =
    foldl' update grid (keys xy1 xy2)
    where f _ x = Just (1 - x)
          update g k = Map.updateWithKey f k g

turnOff :: XY -> XY -> Grid -> Grid
turnOff xy1 xy2 grid =
    foldl' update grid (keys xy1 xy2)
    where
        update g k = Map.adjust (const 0) k g

turnOn :: XY -> XY -> Grid -> Grid
turnOn xy1 xy2 grid =
    foldl' update grid (keys xy1 xy2)
    where
        update g k = Map.adjust (const 1) k g

lights :: [(XY, Int)]
lights = [((x,y),0) | x <- [0..(size-1)], y <-[0..(size-1)]]

lightsMap :: Grid
lightsMap = Map.fromList lights

numLit = Map.foldl' (+) 0

evalInst :: Instruction -> Grid -> Grid
evalInst (inst, xy1, xy2) g = case inst of
    "turn off" -> turnOff xy1 xy2 g
    "turn on"  -> turnOn xy1 xy2 g
    "toggle"   -> toggle xy1 xy2 g

main :: IO ()
main = do
    input <- readFile "./2015/day6/day.txt"
    dataSet <- case parse inputParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x
    let finalLightsMap = foldl' (flip evalInst) lightsMap dataSet
    print $ numLit lightsMap
    print "Answer Part 1:"
    print $ numLit finalLightsMap


displayMap :: Grid -> IO ()
displayMap grid = mapM_ putStrLn [ row y | y <- [0..(size - 1)] ]
  where
    row y = [ display (Map.findWithDefault 0 (x, y) grid) | x <- [0..(size - 1)] ]
    display 0 = '.'  -- off
    display _ = '#'  -- on


