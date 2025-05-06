--- Day 4: The Ideal Stocking Stuffer ---

-- Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically forward-thinking little girls and boys.

-- To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. The input to the MD5 hash is some secret key (your puzzle input, given below) followed by a number in decimal. To mine AdventCoins, you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.

-- For example:

--     If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such number to do so.
    -- If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....

-- Your puzzle input is iwrupvqb.

import Crypto.Hash
import qualified Data.ByteString.Char8 as BS

adventMine :: Show a => String -> String -> a -> Bool
adventMine zs key = (==zs) . take (length zs) . show . hashWith MD5 . BS.pack . (key <>) . show

dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ []     = []
dropUntil p (x:xs)
  | p x            = xs
  | otherwise      = dropUntil p xs

main :: IO ()
main = do
    let myKey = "iwrupvqb"
    print "Answer part 1:"
    let ans1 = head $ dropUntil (adventMine "00000" myKey) [0..] 
    print $ ans1 - 1
    print "Answer part 2:"
    let ans2 =  head $ dropUntil (adventMine "000000" myKey) [ans1..] 
    print $ ans2 - 1