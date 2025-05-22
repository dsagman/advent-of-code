-- The elves bought too much eggnog again - 150 liters this time. To fit it all into your refrigerator, you'll need to move it into smaller containers. You take an inventory of the capacities of the available containers.

-- For example, suppose you have containers of size 20, 15, 10, 5, and 5 liters. If you need to store 25 liters, there are four ways to do it:

--     15 and 10
--     20 and 5 (the first 5)
--     20 and 5 (the second 5)
--     15, 5, and 5

-- Filling all containers entirely, how many different combinations of containers can exactly fit all 150 liters of eggnog?

import Data.List

test :: [Int]
test = [20, 15, 10, 5, 5]

testnog :: Int
testnog = 25

eggnog :: Int
eggnog = 150 -- real data

part1 :: Int -> [Int] -> [[Int]]
part1 vol = filter ((==vol) . sum) . subsequences 

part2 :: Int -> [Int] -> [(Int, [Int])]
part2 vol xs = filter ((==m) . fst) (zip ls p1)
    where p1 = part1 vol xs
          ls = map length p1
          m  = minimum ls

main :: IO ()
main = do
    input <- map (\x -> read x :: Int) . lines <$> readFile "./2015/day17/day.txt"
    print "Part 1:"
    print $ length (part1 eggnog input)
    print "Part 2:"
    print $ length (part2 eggnog input)
