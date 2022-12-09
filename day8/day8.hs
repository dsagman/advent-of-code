module Main where
 
import Data.List 
import Data.List.Split 


main :: IO ()
main = do
        dataFile <- readFile "day8/input.txt" 
        -- dataFile <- readFile "day8/test.txt" 
        let trees = (strLstToIntLst . lines) dataFile

        let fromLeft  = map lookLst trees
        let fromRight = map lookLst $ map reverse trees
        let fromTop   = map lookLst $ transpose trees
        -- let fromBot   = map lookLst $ (reverse . transpose) trees
        let fromBot   = map lookLst $ map reverse $ transpose trees

        -- mapM_ print fromLeft
        mapM_ print trees
        print ""
        mapM_ print (map reverse trees)

        let gridLeft  = fromLeft 
        let gridRight = map reverse fromRight
        let gridTop   = transpose fromTop 
        let gridBot   = (reverse . transpose) fromBot 

        print "from left"
        mapM_ print $ gridLeft
        print "from right"
        mapM_ print $ gridRight
        print "from top"
        mapM_ print $ gridTop
        print "from bottom"
        mapM_ print $ gridBot

        let grids = [gridLeft, gridRight, gridTop, gridBot]
        let combined = foldl1 add2dLst grids
        print "combined"
        mapM_ print $ combined

        print $ length $ concatMap (filter (>0)) combined

    
        let answer1 =  "no"
        let answer2 = "dice"
        putStr "Part 1 answer: " 
        print answer1

        putStr "Part 2 answer: " 
        print answer2

-- 30373
-- 25512
-- 65332
-- 33549
-- 35390

add2dLst = zipWith (zipWith(+))

lookLst xs =  (map isTaller . tail . inits) xs

isTaller xs 
    | length xs == 1 = 1 
    | otherwise = if aTree > (maximum upTo) then 1 else 0
    where 
          upTo = init xs
          aTree = last xs

makeGrid :: [Int] -> Int -> [[Int]]
makeGrid xs gSize = [(take n $ repeat 1) ++ (take (gSize - n) $ repeat 0) | n <- xs]

strLstToIntLst :: [String] -> [[Int]]
strLstToIntLst = (map . map) (\x -> read [x] :: Int)