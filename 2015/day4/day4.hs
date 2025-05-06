--- Day 4: The Ideal Stocking Stuffer ---

-- Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically forward-thinking little girls and boys.

-- To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. The input to the MD5 hash is some secret key (your puzzle input, given below) followed by a number in decimal. To mine AdventCoins, you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.

-- For example:

--     If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such number to do so.
    -- If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....

-- Your puzzle input is iwrupvqb.

import Crypto.Hash
import qualified Data.ByteString.Char8 as BS
-- import Data.ByteString (ByteString)

adventMine :: String -> Int -> [Digest MD5]
adventMine key startN = map (hashWith MD5 . BS.pack . (key <>) . show) [startN..]

adventNonce :: (Show a) => Int -> String -> [a] -> Int
adventNonce n z (x:xs) = 
    if take 5 (show x) == z
    then n
    else adventNonce (n+1) z xs

main :: IO ()
main = do
    let myKey = "iwrupvqb"
    print "Answer part 1:"
    print $ adventNonce 0 "00000" $ adventMine myKey 0
    print "Answer part 2:"
    print $ adventNonce 0 "000000" $ adventMine myKey 0