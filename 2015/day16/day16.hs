-- Your Aunt Sue has given you a wonderful gift, and you'd like to send her a thank you card. However, there's a small problem: she signed it "From, Aunt Sue".

-- You have 500 Aunts named "Sue".

-- So, to avoid sending the card to the wrong person, you need to figure out which Aunt Sue (which you conveniently number 1 to 500, for sanity) gave you the gift. You open the present and, as luck would have it, good ol' Aunt Sue got you a My First Crime Scene Analysis Machine! Just what you wanted. Or needed, as the case may be.

-- The My First Crime Scene Analysis Machine (MFCSAM for short) can detect a few specific compounds in a given sample, as well as how many distinct kinds of those compounds there are. According to the instructions, these are what the MFCSAM can detect:

--     children, by human DNA age analysis.
--     cats. It doesn't differentiate individual breeds.
--     Several seemingly random breeds of dog: samoyeds, pomeranians, akitas, and vizslas.
--     goldfish. No other kinds of fish.
--     trees, all in one group.
--     cars, presumably by exhaust or gasoline or something.
--     perfumes, which is handy, since many of your Aunts Sue wear a few kinds.

-- In fact, many of your Aunts Sue have many of these. You put the wrapping from the gift into the MFCSAM. It beeps inquisitively at you a few times and then prints out a message on ticker tape:

-- children: 3
-- cats: 7
-- samoyeds: 2
-- pomeranians: 3
-- akitas: 0
-- vizslas: 0
-- goldfish: 5
-- trees: 3
-- cars: 2
-- perfumes: 1

-- You make a list of the things you can remember about each Aunt Sue. Things missing from your list aren't zero - you simply don't remember the value.

-- What is the number of the Sue that got you the gift?
--- Part Two ---

-- As you're about to send the thank you note, something in the MFCSAM's instructions catches your eye. Apparently, it has an outdated retroencabulator, and so the output from the machine isn't exact values - some of them indicate ranges.

-- In particular, the cats and trees readings indicates that there are greater than that many (due to the unpredictable nuclear decay of cat dander and tree pollen), while the pomeranians and goldfish readings indicate that there are fewer than that many (due to the modial interaction of magnetoreluctance).

-- What is the number of the real Aunt Sue?


import Data.Char
import Data.List.Split
import Data.Maybe

import qualified Data.HashMap.Strict as Map


myAunt :: Map.HashMap String Int
myAunt = Map.fromList
    [("sue"        , 0),
    ("children"    , 3),
    ("cats"        , 7),
    ("samoyeds"    , 2),
    ("pomeranians" , 3),
    ("akitas"      , 0),
    ("vizslas"     , 0),
    ("goldfish"    , 5),
    ("trees"       , 3),
    ("cars"        , 2),
    ("perfumes"    , 1)]

f1 :: String -> Int -> Int -> (Bool, Int)
f1 k al ar 
    | k == "sue" = (True, ar)
    | otherwise  = (al == ar, ar)

f2 :: String -> Int -> Int -> (Bool, Int)
f2 k al ar 
    | k == "sue"         = (True, ar)
    | k == "cats"        = (al < ar, ar) 
    | k == "trees"       = (al < ar, ar)
    | k == "goldfish"    = (al > ar, ar) 
    | k == "pomeranians" = (al > ar, ar) 
    | otherwise          = (al == ar, ar)

isMyAunt :: (String -> Int -> Int -> (Bool, Int)) -> 
            [Map.HashMap String Int] -> [Int]
isMyAunt fn m = map (snd . fromJust . Map.lookup "sue") $
                filter (all fst . Map.elems) $ 
                map (Map.intersectionWithKey fn myAunt) m

main :: IO ()
main = do
    input <- lines <$> readFile "./2015/day16/day.txt"
    let dataSet = map (Map.fromList .
                       map (\(x:y:_) -> (x, read y :: Int)) .
                       chunksOf 2 .
                       filter (not . null) .
                       splitOneOf ":, " .
                       map toLower) input
    putStrLn "My aunt"
    print "Answer Part 1:"
    print $ isMyAunt f1 dataSet

    print "Answer Part 2:"
    print $ isMyAunt f2 dataSet



