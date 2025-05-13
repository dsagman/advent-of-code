import Data.List (group)

say :: [Int] -> [Int]
say xs = concat [length x : [head x] | x <- group xs]

main :: IO ()
main = do
    let x = [1,3,2,1,1,3,1,1,1,2]
    let sayX = iterate say x
    
    let part1 = map length $ take 41 sayX
    print "Answer Part 1:"
    mapM_ print $ zip [0..] part1

    let part2 = map length $ take 51 sayX
    print "Answer Part 1:"
    mapM_ print $ zip [0..] part2