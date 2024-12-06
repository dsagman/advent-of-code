import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

type GameState = State (Map (Int, Int) Char, ((Int, Int), Char)) -- (grid, ((x, y), dir))

main :: IO ()
main = do
    input <- lines <$> readFile "2024/day6/day.txt"
    -- input <- lines <$> readFile "2024/day6/test"

    let grid = Map.fromList $ [( (x, y), c) | (y, row) <- zip [0..] input, (x, c) <- zip [0..] row]
    let start = (head $ Map.keys $ Map.filter (== '^') grid, '^')
    let maxX = maximum $ map fst $ Map.keys grid
    let maxY = maximum $ map snd $ Map.keys grid

    let finalState = execState (do
            forward
            ) (grid, start)

    let (finalGrid, finalPlayerState) = finalState
    let visited = [value | (pos, value) <- Map.toList finalGrid, value `elem` "^v<>"]
    printGrid finalGrid
    print finalPlayerState
    -- 206 is too low
    -- 5243 is too high
    print visited
    print $ length visited
    putStrLn "done"

forward :: GameState ()
forward = do
    (grid, ((x, y), dir)) <- get
    let maxX = maximum $ map fst $ Map.keys grid
    let maxY = maximum $ map snd $ Map.keys grid

    -- Check if the player is out of bounds
    when (x >= 0 && x < maxX && y >= 0 && y < maxY) $ do
        tryMove 4
        -- ok <- move
        -- if ok
        --     then forward
        --     else do
        --         rotate90
        --         forward
        --         return ()
        
        
tryMove :: Int -> GameState ()
tryMove 0 = return () -- Stop if all directions have been tried
tryMove attempts = do
    ok <- move
    if ok
        then forward -- Continue moving in the current direction
        else do
            rotate90 -- Rotate 90 degrees clockwise if blocked
            tryMove (attempts - 1)

rotate90 :: GameState ()
rotate90 = do
    (grid, ((x, y), dir)) <- get
    let newDir = case dir of
                    '^' -> '>'
                    '>' -> 'v'
                    'v' -> '<'
                    '<' -> '^'
                    _   -> dir
    put (grid, ((x, y), newDir))

move :: GameState Bool
move = do
    (grid, ((x, y), dir)) <- get
    let newPos = case dir of
                    '^' -> (x, y - 1)
                    'v' -> (x, y + 1)
                    '<' -> (x - 1, y)
                    '>' -> (x + 1, y)
                    _   -> (x, y) -- Fallback for invalid direction 
    -- Check if the new position is valid
    if Map.findWithDefault '.' newPos grid /= '#'
            then do
                let updatedGrid = Map.insert (x, y) dir (Map.insert newPos dir grid)
                put (updatedGrid, (newPos, dir))
                return True
            else return False -- Stop if obstacle or boundary is reached    

-- Change direction
changeDir :: Char -> GameState ()
changeDir newDir = do
    (grid, ((x, y), _)) <- get
    put (grid, ((x, y), newDir))

-- Print the grid
printGrid :: Map (Int, Int) Char -> IO ()
printGrid grid =
    mapM_ print [ [ Map.findWithDefault ' ' (x, y) grid | x <- [0..maxX] ] | y <- [0..maxY] ]
    where
        maxX = maximum $ map fst $ Map.keys grid
        maxY = maximum $ map snd $ Map.keys grid
