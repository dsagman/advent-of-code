module Main where
 
import Data.List ( inits, transpose ) 


main :: IO ()
main = do
        dataFile <- readFile "day8/input.txt" 
        -- dataFile <- readFile "day8/test.txt" 
        let trees = (strLstToIntLst . lines) dataFile

        let fromLeft  = map lookLst trees
        let fromRight = map lookLst $ map reverse trees
        let fromTop   = map lookLst $ transpose trees
        let fromBot   = map lookLst $ map reverse $ transpose trees

        let gridLeft  = fromLeft 
        let gridRight = map reverse fromRight
        let gridTop   = transpose fromTop 
        let gridBot   = (reverse . transpose) fromBot 

        let combined = foldl1 add2dLst [gridLeft, gridRight, gridTop, gridBot]
        let answer1 = length $ concatMap (filter (>0)) combined

        let fromLeft2  = map lookLst2 trees
        let fromRight2 = map lookLst2 $ map reverse trees
        let fromTop2   = map lookLst2 $ transpose trees
        let fromBot2   = map lookLst2 $ map reverse $ transpose trees

        let gridLeft2  = fromLeft2 
        let gridRight2 = map reverse fromRight2
        let gridTop2   = transpose fromTop2 
        let gridBot2   = (reverse . transpose) fromBot2 

        let combined2 = foldl1 mul2dLst [gridLeft2, gridRight2, gridTop2, gridBot2]

        let answer2 = (maximum . map maximum) combined2

        putStr "Part 1 answer: " 
        print answer1

        putStr "Part 2 answer: " 
        print answer2


add2dLst :: [[Int]] -> [[Int]] -> [[Int]]
add2dLst = zipWith (zipWith(+))

mul2dLst :: [[Int]] -> [[Int]] -> [[Int]]
mul2dLst = zipWith (zipWith(*))

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs) = x : if p x then takeUntil p xs else []

lookLst :: (Num b, Ord a) => [a] -> [b]
lookLst xs =  (map isTaller . tail . inits) xs

isTaller :: (Num a1, Ord a2) => [a2] -> a1
isTaller xs 
    | length xs == 1 = 1 
    | otherwise = if aTree > (maximum upTo) then 1 else 0
    where 
          upTo = init xs
          aTree = last xs

lookLst2 :: Ord a => [a] -> [Int]
lookLst2 xs = (map canSee . tail . inits) xs

canSee :: Ord a => [a] -> Int
canSee xs 
    | length xs == 1 = 0
    | otherwise = length $ takeUntil (<aTree) upTo
    where 
          upTo = (reverse . init) xs
          aTree = last xs


makeGrid :: [Int] -> Int -> [[Int]]
makeGrid xs gSize = [(take n $ repeat 1) ++ (take (gSize - n) $ repeat 0) | n <- xs]

strLstToIntLst :: [String] -> [[Int]]
strLstToIntLst = (map . map) (\x -> read [x] :: Int)