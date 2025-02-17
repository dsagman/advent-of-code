module Main where
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, empty)
import Data.Void
import Data.Maybe
import Data.Either
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map



type Parser = Parsec Void String

data Robot = Robot { x :: Int, y :: Int, vx :: Int, vy :: Int }
    deriving (Show)

data Room = Room { width :: Int, height :: Int}
    deriving (Show)

isDigitOrMinus :: Char -> Bool
isDigitOrMinus c = c `elem` '-':['0'..'9']


pRobot :: Parser Robot
pRobot = do
    string "p="
    x <- takeWhileP (Just "x") isDigitOrMinus
    string ","
    y <- takeWhileP (Just "y") isDigitOrMinus
    string " v="
    vx <- takeWhileP (Just "vx") isDigitOrMinus
    string ","
    vy <- takeWhileP (Just "vy") isDigitOrMinus
    return $ Robot (read x) (read y) (read vx) (read vy)


pRobots :: Parser [Maybe Robot]
pRobots = optional pRobot`sepBy` newline

main :: IO ()
main = do
    -- let dataFile = "2024/day13/test"

    -- let room = Room { width = 11, height = 7 }
    -- let dataFile = "2024/day14/test"

    let room = Room { width = 101, height = 103 }
    let dataFile = "2024/day14/day.txt"
    input <- readFile dataFile
    let parsed = parse pRobots "" input
    case parsed of
        Left bundle -> do
            putStr $ errorBundlePretty bundle
            error "parse error"
        Right robots -> when (Prelude.null (head robots)) $ print "No parse"
    let robots = catMaybes $ fromRight [] parsed

    let n = 100
    let afterNmove = runReader (traverse (moveRobotN n) robots) room

    -- let afterNmoveGrid = runReader (showRobots afterNmove) room
    -- putStr afterNmoveGrid

    let quads = runReader (robotByQuadrant afterNmove) room
    print $ "Part 1 = " ++ show (product quads)
    -- let (robots', n') = runReader (part2 (robots, 1)) room
    let (robots', n') = runReader (runStateT (part2 robots) 0) room
    putStrLn $ runReader (showRobots robots') room
    print $ "Part 2 Moves = " ++ show n'


-- part2 :: ([Robot], Integer) -> Reader Room ([Robot], Integer)
-- part2 (robots, n) = do
--     robots' <- traverse moveRobot robots
--     let maxCount = maximum $ Map.elems $ countRobots robots'
--     if maxCount == 1 
--         then return (robots',n) 
--         else part2 (robots',n+1)

-- With StateT and Reader monads!!!!!
part2 :: [Robot] -> StateT Integer (Reader Room) [Robot]
part2 robots = do
    robots' <- lift $ traverse moveRobot robots  -- Move robots inside the Reader monad
    let maxCount = maximum $ Map.elems $ countRobots robots'
    modify (+1)  -- Increment state  
    if maxCount == 1
        then return robots'  
        else part2 robots'   


robotByQuadrant :: [Robot] -> Reader Room [Int]
robotByQuadrant robots = do
    Room width height <- ask
    let xMiddle = width `div` 2
    let yMiddle = height `div` 2
    let quad1 = length $ filter (\(Robot x y _ _) -> x > xMiddle && y > yMiddle) robots
    let quad2 = length $ filter (\(Robot x y _ _) -> x < xMiddle && y > yMiddle) robots
    let quad3 = length $ filter (\(Robot x y _ _) -> x < xMiddle && y < yMiddle) robots
    let quad4 = length $ filter (\(Robot x y _ _) -> x > xMiddle && y < yMiddle) robots
    return [quad1, quad2, quad3, quad4]

moveRobotN :: Int -> Robot -> Reader Room Robot
moveRobotN n robot = foldl (>>=) (pure robot) (replicate n moveRobot)

moveRobot :: Robot -> Reader Room Robot
moveRobot robot = do
    Room width height <- ask
    let Robot x y vx vy = robot
    let newX = (x + vx) `mod` width 
    let newY = (y + vy) `mod` height 
    return $ Robot newX newY vx vy

showRobots :: [Robot] -> Reader Room String
showRobots robots = do
    Room width height <- ask
    let emptyGrid = replicate height (replicate width '0')  -- Start with '0' for empty spaces
        robotCounts = countRobots robots
        gridWithRobots = foldl (updateGrid robotCounts) emptyGrid [(x, y) | y <- [0..height-1], x <- [0..width-1]]
    return $ unlines gridWithRobots

countRobots :: [Robot] -> Map.Map (Int, Int) Int
countRobots = foldl (\m (Robot x y _ _) -> Map.insertWith (+) (x, y) 1 m) Map.empty

updateGrid :: Map.Map (Int, Int) Int -> [[Char]] -> (Int, Int) -> [[Char]]
updateGrid robotCounts grid (x, y) =
    let count = Map.findWithDefault 0 (x, y) robotCounts
        char = if count == 0 then '.' else head (show count)
    in take y grid ++
       [take x (grid !! y) ++ [char] ++ drop (x + 1) (grid !! y)] ++
       drop (y + 1) grid
