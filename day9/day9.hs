module Main where
import Data.List ( group, nub )

type Pos = (Int, Int)

main :: IO ()
main = do
        dataFile <- readFile "day9/input.txt" 
        -- dataFile <- readFile "day9/test.txt" 
        -- dataFile <- readFile "day9/test2.txt" 
        let moves = lines dataFile

        let hMoves = allMoves (0,0) moves
        let tMoves = allFollowT hMoves
        let answer1 = length (nub tMoves)

        let nineTMoves = (map head . group) $ iterate allFollowT hMoves !! 9
        let answer2 = (length . nub) nineTMoves

        putStr "Part 1 answer: " 
        print answer1

        putStr "Part 2 answer: " 
        print answer2

allFollowT :: [Pos] -> [Pos]
allFollowT hs = scanl (\x y -> (followT x y)) (0,0) hs

followT :: Pos -> Pos -> Pos
followT (tX, tY) (hX, hY) 
    | (dX > 1) && (dY > 1)   = (hX + 1, hY + 1)
    | (dX > 1) && (dY < -1)  = (hX + 1, hY - 1)
    | (dX < -1) && (dY > 1)  = (hX - 1, hY + 1)
    | (dX < -1) && (dY < -1) = (hX - 1, hY - 1)
    | (dX > 1)  = (hX + 1, hY)
    | (dX < -1) = (hX - 1, hY)
    | (dY > 1)  = (hX, hY + 1)
    | (dY < -1) = (hX, hY - 1)
    | otherwise = (tX, tY)
    where dX = tX - hX
          dY = tY - hY

allMoves :: Pos -> [String] -> [Pos]
allMoves xy [] = []
allMoves (curX, curY) (m:ms) = (curX, curY) : (oneMoves) ++ allMoves (newX, newY) ms
    where oneMoves = byOneMove (curX, curY) m 
          (newX, newY) = (last) oneMoves

byOneMove :: Pos -> [Char] -> [Pos]
byOneMove (curX, curY) aMove = 
    case head aMove of
        'R' -> zipWith (\x y -> (x,y)) (zipWith (+) (repeat curX) n) (repeat curY)
        'U' -> zipWith (\x y -> (x,y)) (repeat curX) (zipWith (+) (repeat curY) n)
        'L' -> zipWith (\x y -> (x,y)) (zipWith (-) (repeat curX) n) (repeat curY)
        'D' -> zipWith (\x y -> (x,y)) (repeat curX) (zipWith (-) (repeat curY) n)
        where n = [1..(\x -> read x :: Int) $ drop 2 aMove]