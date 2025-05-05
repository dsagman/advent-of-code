{-# LANGUAGE TupleSections #-}
-- Santa is delivering presents to an infinite two-dimensional grid of houses.

-- He begins by delivering a present to the house at his starting location, and then an elf at the North Pole calls him via radio and tells him where to move next. Moves are always exactly one house to the north (^), south (v), east (>), or west (<). After each move, he delivers another present to the house at his new location.

-- However, the elf back at the north pole has had a little too much eggnog, and so his directions are a little off, and Santa ends up visiting some houses more than once. How many houses receive at least one present?

-- For example:

--     > delivers presents to 2 houses: one at the starting location, and one to the east.
--     ^>v< delivers presents to 4 houses in a square, including twice to the house at his starting/ending location.
--     ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.


--- Part Two ---

-- The next year, to speed up the process, Santa creates a robot version of himself, Robo-Santa, to deliver presents with him.

-- Santa and Robo-Santa start at the same location (delivering two presents to the same starting house), then take turns moving based on instructions from the elf, who is eggnoggedly reading from the same script as the previous year.

-- This year, how many houses receive at least one present?

-- For example:

--     ^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa goes south.
--     ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up back where they started.
--     ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one direction and Robo-Santa going the other.


import Data.Bifunctor
import Data.List

parseDir :: Char -> (Int, Int)
parseDir dir =
    case dir of
        '^' -> (0,1)
        'v' -> (0,-1)
        '>' -> (1,0)
        '<' -> (-1,0)


-- assume sorted house locations
houseCounts :: [(Int,(Int, Int))] -> [(Int,(Int, Int))]
houseCounts [x] = [x]
houseCounts (h1:h2:hs) =
    if h1xy == h2xy
       then houseCounts ((h1c+1,h1xy) : hs)
        else h1: houseCounts (h2:hs)
    where (h1c, h1xy) = h1
          (h2c, h2xy) = h2

everyOther :: [a] -> [a]
everyOther [x] = [x]
everyOther (x:y:xs) = x:everyOther xs
everyOther _ = []

xys :: [(Int, Int)] -> [(Int, (Int, Int))]
xys = scanl (\(_, (x, y)) (dx, dy) -> (1, (x + dx, y + dy))) (1, (0, 0))
 

main :: IO ()
main = do
    input <- readFile "./2015/day3/day.txt"
    let dirs = map parseDir input
    print "Part 1 answer:"
    let ans1 = (length . map fst . houseCounts . sort . xys) dirs
    print ans1
    print "Part 2 answer:"
    let santa = (xys . everyOther) dirs
    let robot = (xys . everyOther) (drop 1 dirs)
    let ans2 = (length . map fst . houseCounts . sort) (santa <> robot)
    print ans2
