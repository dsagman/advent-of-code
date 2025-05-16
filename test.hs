import Data.List
import Data.Maybe
s = "This string contains the numbers 7, 11, and 42."

t = tails

-- r :: [String] -> Int
-- r xs = reads xs :: Int 

r :: [Char] -> [(Int, String)]
r = concatMap reads . tails

g = listToMaybe . r

f = unfoldr g
