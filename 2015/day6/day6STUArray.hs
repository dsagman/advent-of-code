import Control.Monad
import Control.Monad.ST
import Data.Array.Unboxed
import Data.Array.ST
import Data.Ix
import Data.List (foldl')
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)

type Cmd = (String, XY, XY)
type XY = (Int, Int)
type Grid = UArray (Int, Int) Int
type MGrid s = STUArray s (Int, Int) Int

size :: Int
size = 999
boundsXY :: ((Int, Int), (Int, Int))
boundsXY = ((0, 0), (size, size))


cmdParser :: Parser Cmd
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
inputParser = many (try (cmdParser <* newline))

evalPart1M :: Cmd -> MGrid s -> ST s ()
evalPart1M (inst, (x1, y1), (x2, y2)) arr =
    forM_ [(x, y) | x <- [x1..x2], y <- [y1..y2]] $ \(x, y) -> do
        case inst of
            "turn off" -> writeArray arr (x, y) 0
            "turn on"  -> writeArray arr (x, y) 1
            "toggle"   -> do
                v <- readArray arr (x, y)
                writeArray arr (x, y) (1 - v)
            _ -> pure ()

evalPart2M :: Cmd -> MGrid s -> ST s ()
evalPart2M (inst, (x1, y1), (x2, y2)) arr =
    forM_ [(x, y) | x <- [x1..x2], y <- [y1..y2]] $ \(x, y) -> do
        v <- readArray arr (x, y)
        let v' = case inst of
                    "turn off" -> max 0 (v - 1)
                    "turn on"  -> v + 1
                    "toggle"   -> v + 2
                    _ -> v
        writeArray arr (x, y) v'

runPart :: (Cmd -> MGrid s -> ST s ()) -> [Cmd] -> ST s Grid
runPart step cmds = do
    arr <- newArray boundsXY 0
    mapM_ (`step` arr) cmds
    freeze arr

numLit :: Grid -> Int
numLit = sum . elems

main :: IO ()
main = do
    input <- readFile "./2015/day6/day.txt"
    dataSet <- case parse inputParser "" input of
        Left err -> putStrLn ("Error parsing input: " ++ show err) >> exitFailure
        Right x  -> pure x

    let part1 = runST $ runPart evalPart1M dataSet
    putStrLn "Answer Part 1:"
    print $ numLit part1

    let part2 = runST $ runPart evalPart2M dataSet
    putStrLn "Answer Part 2:"
    print $ numLit part2
