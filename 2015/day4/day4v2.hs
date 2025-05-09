--- Day 4: The Ideal Stocking Stuffer ---

-- Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts for all the economically forward-thinking little girls and boys.

-- To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. The input to the MD5 hash is some secret key (your puzzle input, given below) followed by a number in decimal. To mine AdventCoins, you must find Santa the lowest positive number (no leading zeroes: 1, 2, 3, ...) that produces such a hash.

-- For example:

--     If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such number to do so.
    -- If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....

-- Your puzzle input is iwrupvqb.

import Crypto.Hash
import qualified Data.ByteString.Char8 as BS


adventHash :: Show a => String -> a -> String
adventHash key n = show $ hashWith MD5 (BS.pack (key <> show n))

adventNonce5 :: (Show t, Num t) => t -> String -> t
adventNonce5 n key = 
    if take 5 (adventHash key n) == "00000"
    then n
    else adventNonce5 (n+1) key

adventNonce6 :: (Show t, Num t) => t -> String -> t
adventNonce6 n key = 
    if take 6 (adventHash key n) == "000000"
    then n
    else adventNonce6 (n+1) key

main :: IO ()
main = do
    let myKey = "iwrupvqb"
    print "V2 Answer part 1:"
    print $ adventNonce5 0 myKey
    print "V2 Answer part 2:"
    print $ adventNonce6 0 myKey