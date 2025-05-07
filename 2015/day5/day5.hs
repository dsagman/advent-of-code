{-# LANGUAGE RecordWildCards #-}
-- A nice string is one with all of the following properties:

--     It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
--     It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
--     It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements.

-- For example:

--     ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
--     aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
--     jchzalrnumimnmhp is naughty because it has no double letter.
--     haegwjzuvuyypxyu is naughty because it contains the string xy.
--     dvszwmarrgswjxmb is naughty because it contains only one vowel.

-- How many strings are nice?

--- Part Two ---

-- Realizing the error of his ways, Santa has switched to a better model of determining whether a string is naughty or nice. None of the old rules apply, as they are all clearly ridiculous.
-- Now, a nice string is one with all of the following properties:
-- It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
-- It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.
-- For example:

-- qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) and a letter that repeats with exactly one letter between them (zxz).
-- xxyxx is nice because it has a pair that appears twice and a letter that repeats with one between, even though the letters used by each rule overlap.
-- uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single letter between them.
-- ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but no pair that appears twice.


import Control.Monad.State
import Data.List

data Nice = Nice {prev :: Char, vowels :: Int, pair :: Bool, naughty :: Bool}
    deriving (Show, Eq)

parseNice :: String -> State Nice Char
parseNice [] = pure ' '
parseNice (x:xs) = do
    Nice {..} <- get
    let newPair = pair || (x == prev)
    let newVowels = if x `elem` "aeiou" then vowels + 1 else vowels
    let newNaughty = naughty || (prev:[x]) `elem` ["ab", "cd", "pq", "xy"]
    put (Nice x newVowels newPair newNaughty)
    parseNice xs

validNice Nice {..} = (vowels >= 3) && pair && not naughty

data Nice2 = Nice2 {pMinus1 :: Char, pMinus2 :: Char, pMinus3 :: Char, doubles :: [String], rPair :: Bool}
    deriving (Show, Eq)

parseNice2 :: String -> State Nice2 Char
parseNice2 [] = pure ' '
parseNice2 (x:xs) = do
    Nice2 {..} <- get
    let newDoubles = if (x /= pMinus1) || (pMinus1 /= pMinus2) || (pMinus2 == pMinus3)
                    then (pMinus1 :[x]) : doubles
                    else doubles
    let newRPair = rPair || (x == pMinus2)
    put (Nice2 x pMinus1 pMinus2 newDoubles newRPair)
    parseNice2 xs

validNice2 Nice2 {..} = rPair && (length doubles /= length (nub doubles))

main :: IO ()
main = do
    let initNice = Nice ' ' 0 False False
    inputLines <- lines <$> readFile "./2015/day5/day.txt"
    let nices = [validNice $ execState (parseNice i) initNice | i <- inputLines]
    let ans1 = (length . filter id) nices
    print "Answer part 1:"
    print ans1

    let initNice2 = Nice2 '?' '!' '.' [] False
    let nices2 = [validNice2 $ execState (parseNice2 i) initNice2 | i <- inputLines]
    let ans2 = (length . filter id) nices2
    print "Answer part 2:"
    print ans2


    -- print $ validNice2 $ execState (parseNice2 "qjhvhtzxzqqjkmpb") initNice2 -- True
    -- print $ validNice2 $ execState (parseNice2 "xxyxx") initNice2    -- True
    -- print $ validNice2 $ execState (parseNice2 "uurcxstgmygtbstg") initNice2  -- False
    -- print $ validNice2 $ execState (parseNice2 "ieodomkazucvgmuy") initNice2  -- False

    -- print $ validNice $ execState (parseNice "ugknbfddgicrmopn") initNice -- True
    -- print $ validNice $ execState (parseNice "aaa") initNice -- True
    -- print $ validNice $ execState (parseNice "jchzalrnumimnmhp") initNice -- False
    -- print $ validNice $ execState (parseNice "haegwjzuvuyypxyu") initNice -- False
    -- print $ validNice $ execState (parseNice "dvszwmarrgswjxmb") initNice -- False