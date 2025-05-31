{-
--- Day 20: Infinite Elves and Infinite Houses ---

To keep the Elves busy, Santa has them deliver some presents by hand, door-to-door. He sends them down a street with infinite houses numbered sequentially: 1, 2, 3, 4, 5, and so on.

Each Elf is assigned a number, too, and delivers presents to houses based on that number:

    The first Elf (number 1) delivers presents to every house: 1, 2, 3, 4, 5, ....
    The second Elf (number 2) delivers presents to every second house: 2, 4, 6, 8, 10, ....
    Elf number 3 delivers presents to every third house: 3, 6, 9, 12, 15, ....

There are infinitely many Elves, numbered starting with 1. Each Elf delivers presents equal to ten times his or her number at each house.

So, the first nine houses on the street end up like this:

House 1 got 10 presents.
House 2 got 30 presents.
House 3 got 40 presents.
House 4 got 70 presents.
House 5 got 60 presents.
House 6 got 120 presents.
House 7 got 80 presents.
House 8 got 150 presents.
House 9 got 130 presents.

The first house gets 10 presents: it is visited only by Elf 1, which delivers 1 * 10 = 10 presents. The fourth house gets 70 presents, because it is visited by Elves 1, 2, and 4, for a total of 10 + 20 + 40 = 70 presents.

What is the lowest house number of the house to get at least as many presents as the number in your puzzle input?
-}

-- Your puzzle input is 29000000.
import Data.List 

data Wheel = Wheel Integer [Integer]

myInput = 29000000

main :: IO ()
main = do

    -- let part1 = takeWhile (<(myInput `div` 10)) $ map a000203 [2..]
    let part1 = takeWhile (<=2900000) $ map a000203 [2..]


    print "Part 1 Answer:"
    print $ length part1
    print "----------"

a000203 n = product $ 
            zipWith (\p e -> (p^(e+1)-1) `div` (p-1)) 
                    (a027748_row n) 
                    (a124010_row n)    

-- a027748 n k = a027748_tabl !! (n-1) !! (k-1)
-- a027748_tabl = map a027748_row [1..]
a027748_row 1 = [1]
a027748_row n = unfoldr fact n where
   fact 1 = Nothing
   fact x = Just (p, until ((> 0) . (`mod` p)) (`div` p) x)
            where p = a020639 x  -- smallest prime factor of x

a020639 n = spf a000040_list where
  spf (p:ps) | n < p^2      = n
             | mod n p == 0 = p
             | otherwise    = spf ps

a000040 n = genericIndex a000040_list (n - 1)

a000040_list = base ++ larger where
    base = [2, 3, 5, 7, 11, 13, 17]
    larger = p : filter prime more
    prime n = all ((> 0) . mod n) $ takeWhile (\x -> x*x <= n) larger
    _ : p : more = roll $ makeWheels base
    roll (Wheel n rs) = [n * k + r | k <- [0..], r <- rs]
    makeWheels = foldl nextSize (Wheel 1 [1])
    nextSize (Wheel size bs) p = Wheel (size * p) [r | k <- [0..p-1], b <- bs, let r = size*k+b, mod r p > 0]

a124010 n k = a124010_tabf !! (n-1) !! (k-1)

a124010_row 1 = [0]
a124010_row n = f n a000040_list where
   f 1 _      = []
   f u (p:ps) = h u 0 where
     h v e | m == 0 = h v' (e + 1)
           | m /= 0 = if e > 0 then e : f v ps else f v ps
           where (v', m) = divMod v p

a124010_tabf = map a124010_row [1..]