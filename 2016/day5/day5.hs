import Crypto.Hash
import qualified Data.ByteString.Char8 as BS
import Numeric
import Data.List
import Data.Array



main :: IO ()
main = do
    -- test
    -- let myKey = "abc" 
    -- let keyList = [3231929,5017308,5278568,5357525,5708769,6082117,8036669,8605828,8609554,8760605,9495334,10767910,11039607,12763908,13666005,13753421,14810753,15274776,15819744,18455182]
    -- part2 = 05ace8e3

    -- puzzle
    let myKey = "ffykfhsq"
    -- let keyList = getKeyList 0 myKey 20
    -- used ^ to pre-compute v
    let keyList = [515840,844745,2968550,4034943,5108969,5257971,5830668,5833677,6497076,6681564,8793263,8962195,10715437,10999728,11399249,12046531,12105075,14775057,15502588,15872452]

    let ans1 = decode1 (take 8 keyList) myKey
    putStrLn $ "Answer part 1:" ++ show ans1

    let ans2 = go2 keyList myKey
    putStrLn $ "Answer part 2:" ++ show ans2

adventMine :: Show a => String -> String -> a -> Bool
adventMine zs key = (==zs) . take (length zs) . show . getHash key

getHash :: Show a => String -> a -> Digest MD5
getHash key offset = hashWith MD5 $ BS.pack (key <> show offset)

getKeyList :: Integer -> String -> Integer -> [Integer]
getKeyList start key 0 = []
getKeyList start key n = ans : getKeyList (ans+1) key (n-1)
    where ans = head $ filter (adventMine "00000" key) [start..]

decode1 :: Show a => [a] -> String -> [Char]
decode1 keyList key = [show (getHash key x) !! 5 | x <- keyList]

insertAt :: Int -> Char -> String -> String
insertAt n x xs = take n xs ++ x : drop (n+1) xs

insertMany :: [[Char]] -> String -> String
insertMany [] xs = xs
insertMany ([i,c]:pairs) xs = 
        if i `elem` ['0'..'7'] && (xs!!ii) == '_' -- don't overwrite
            then insertMany pairs (insertAt ii c xs)
            else insertMany pairs xs
        where ii = read [i] :: Int

decode2 :: Show a => [a] -> [Char] -> String
decode2 keyList key = do
    let codePts = [[i,c] | x <- keyList, let [i,c] = take 2 $ drop 5 $ show (getHash key x) ]
    let password = insertMany codePts "________"
    password

go2 :: [Integer] -> [Char] -> String
go2 keyList key = 
    if '_' `elem` password 
        then go2 extendKeyList key -- need more hashes 
        else password
    where password = decode2 keyList key
          extendKeyList =  keyList ++ getKeyList (1+last keyList) key 1









--- Day 5: How About a Nice Game of Chess? ---

-- You are faced with a security door designed by Easter Bunny engineers that seem to have acquired most of their security knowledge by watching hacking movies.

-- The eight-character password for the door is generated one character at a time by finding the MD5 hash of some Door ID (your puzzle input) and an increasing integer index (starting with 0).

-- A hash indicates the next character in the password if its hexadecimal representation starts with five zeroes. If it does, the sixth character in the hash is the next character of the password.

-- For example, if the Door ID is abc:

--     The first index which produces a hash that starts with five zeroes is 3231929, which we find by hashing abc3231929; the sixth character of the hash, and thus the first character of the password, is 1.
--     5017308 produces the next interesting hash, which starts with 000008f82..., so the second character of the password is 8.
--     The third time a hash starts with five zeroes is for abc5278568, discovering the character f.

-- In this example, after continuing this search a total of eight times, the password is 18f47a30.

-- Given the actual Door ID, what is the password?

-- Your puzzle input is ffykfhsq.

--- Part Two ---

-- As the door slides open, you are presented with a second door that uses a slightly more inspired security mechanism. Clearly unimpressed by the last version (in what movie is the password decrypted in order?!), the Easter Bunny engineers have worked out a better solution.

-- Instead of simply filling in the password from left to right, the hash now also indicates the position within the password to fill. You still look for hashes that begin with five zeroes; however, now, the sixth character represents the position (0-7), and the seventh character is the character to put in that position.

-- A hash result of 000001f means that f is the second character in the password. Use only the first result for each position, and ignore invalid positions.

-- For example, if the Door ID is abc:

--     The first interesting hash is from abc3231929, which produces 0000015...; so, 5 goes in position 1: _5______.
--     In the previous method, 5017308 produced an interesting hash; however, it is ignored, because it specifies an invalid position (8).
--     The second interesting hash is at index 5357525, which produces 000004e...; so, e goes in position 4: _5__e___.

-- You almost choke on your popcorn as the final character falls into place, producing the password 05ace8e3.

-- Given the actual Door ID and this new method, what is the password? Be extra proud of your solution if it uses a cinematic "decrypting" animation.
