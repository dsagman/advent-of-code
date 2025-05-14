-- They have a JSON document which contains a variety of things: arrays ([1,2,3]), objects ({"a":1, "b":2}), numbers, and strings. Your first job is to simply find all of the numbers throughout the document and add them together.

-- For example:

--     [1,2,3] and {"a":2,"b":4} both have a sum of 6.
--     [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
--     {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
--     [] and {} both have a sum of 0.

-- You will not encounter any strings containing numbers.

-- What is the sum of all numbers in the document?

import Text.Parsec
import Text.Parsec.String 
import System.Exit (exitFailure)
import Data.List.Split ( splitOn )
import Data.List

-- Data Structures

data J a = JGroup [J a]
         | JList [J a]
         | JText a
         deriving (Show, Eq)

instance Functor J where
    fmap f (JGroup js) = JGroup (map (fmap f) js)
    fmap f (JList js)  = JList  (map (fmap f) js)
    fmap f (JText j)   = JText (f j)

-- Parser

jParser :: Parser (J String)
jParser = try jGroup
     <|> try jList
     <|> jText

jGroup :: Parser (J String)
jGroup = JGroup <$> between (char '{') (char '}') (many jParser)

jList :: Parser (J String)
jList = JList <$> between (char '[') (char ']') (many jParser)

jText :: Parser (J String)
jText = JText <$> many1 (noneOf "{}[]\n")

inputParser :: Parser (J String)
inputParser = jParser <* newline

-- Printing

showJ :: Int -> J String -> String
showJ lvl (JText js) = indent lvl ++ js
showJ lvl (JGroup (j:js)) = fmtJ lvl "{}" j js
showJ lvl (JList (j:js)) = fmtJ lvl "[]" j js

fmtJ :: Int -> String -> J String -> [J String] -> String
fmtJ lvl (c1:c2) j js= 
    "\n" ++ indent lvl ++ [c1] ++ showJ 0 j ++ 
    concatMap (showJ (lvl+1)) js ++ 
    "\n" ++ indent lvl ++ c2 ++ "\n"

indent :: Int -> String
indent n = replicate (n * 2) ' '  -- 2 spaces per level

-- Sum numbers

sumNums :: String -> Int
sumNums = sum .
           map read .
           filter (not . null) .
           splitOn "," .
           filter (\x -> x `elem` (',':'-':['0'..'9']))
           

sumNoRed :: J String -> Int
sumNoRed (JText j) = sumNums j
sumNoRed (JGroup js)
    | any hasRed js = 0
    | otherwise                 = sum (map sumNoRed js)
  where
    hasRed (JText s) = "red" `isInfixOf` s
    hasRed _         = False
sumNoRed (JList js) = sum (map sumNoRed js)

------ alternate solutions that use functor fmap ----

sumJ :: Num a => J a -> a
sumJ (JText n)   = n
sumJ (JGroup js) = sum (map sumJ js)
sumJ (JList js)  = sum (map sumJ js)

noRedGroups :: J String -> J String
noRedGroups (JText s) = JText s
noRedGroups (JList js) = JList (map noRedGroups js)
noRedGroups (JGroup js)
    | any hasRed js = JGroup []  -- prune the  group
    | otherwise = JGroup (map noRedGroups js)
  where
    hasRed (JText s) = "red" `isInfixOf` s
    hasRed _         = False


main :: IO ()
main = do
    -- input <- readFile "./2015/day12/test.json"    
    input <- readFile "./2015/day12/day.json"
    dataSet <- case parse inputParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x
    putStrLn $ showJ 0 dataSet 

    print "Part 1 Answer:"
    print $ sumNums input
    print $ sumJ $ fmap sumNums dataSet -- using functor

    print "Part 2 Answer:"
    print $ sumNoRed dataSet
    print $ sumJ $ fmap sumNums (noRedGroups dataSet) -- using functor
