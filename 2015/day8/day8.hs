{-# LANGUAGE QuasiQuotes #-}
module Main where
-- For example:

--  "" is 2 characters of code (the two double quotes), but the string contains zero characters.
--  "abc" is 5 characters of code, but 3 characters in the string data.
--  "aaa\"aaa" is 10 characters of code, but the string itself contains six "a" characters and a single, escaped quote character, for a total of 7 characters in the string data.
--  "\x27" is 6 characters of code, but the string itself contains just one - an apostrophe ('), escaped using hexadecimal notation.

-- Santa's list is a file that contains many double-quoted string literals, one on each line. The only escape sequences used are \\ (which represents a single backslash), \" (which represents a lone double-quote character), and \x plus two hexadecimal characters (which represents a single character with that ASCII code).

-- For example, given the four strings above, the total number of characters of string code (2 + 5 + 10 + 6 = 23) minus the total number of characters in memory for string values (0 + 3 + 7 + 1 = 11) is 23 - 11 = 12.

--- Part Two ---

-- Now, let's go the other way. In addition to finding the number of characters of code, you should now encode each code representation as a new string and find the number of characters of the new encoded representation, including the surrounding double quotes.

-- For example:

--  "" encodes to "\"\"", an increase from 2 characters to 6.
--  "abc" encodes to "\"abc\"", an increase from 5 characters to 9.
--  "aaa\"aaa" encodes to "\"aaa\\\"aaa\"", an increase from 10 characters to 16.
--  "\x27" encodes to "\"\\x27\"", an increase from 6 characters to 11.

-- Your task is to find the total number of characters to represent the newly encoded strings minus the number of characters of code in each original string literal. For example, for the strings above, the total encoded length (6 + 9 + 16 + 11 = 42) minus the characters in the original code representation (23, just like in the first part of this puzzle) is 42 - 23 = 19.

rawToStr :: String -> String
rawToStr [] = []
rawToStr (x:xs) = case x of
    '\\' -> e : rawToStr r
    _    -> x : rawToStr xs
    where (e,r) = parseEsc xs

parseEsc :: String -> (Char, String)
parseEsc (x:xs) = case x of
    '\\' -> (x,xs)
    '"'  -> (x,xs)
    'x'  -> ('!', drop 2 xs)
    _    -> ('\\',x:xs)

main :: IO ()
main = do
    -- input <- lines <$> readFile "./2015/day8/test"
    input <- lines <$> readFile "./2015/day8/day.txt"
    let rawSize = map length input
    let chars = map rawToStr input
    let charSize = map ((-2+) . length) chars
    -- mapM_ print $ zip input rawSize
    print "Part 1 Answer:"
    print $ sum rawSize - sum charSize 

    let charsRaw = map show input
    let charsRawSize = map length charsRaw
    -- mapM_ print $ zip charsRaw charsRawSize
    print "Part 2 Answer:"
    print $ sum charsRawSize - sum rawSize
    
