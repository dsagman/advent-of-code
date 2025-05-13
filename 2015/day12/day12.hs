-- They have a JSON document which contains a variety of things: arrays ([1,2,3]), objects ({"a":1, "b":2}), numbers, and strings. Your first job is to simply find all of the numbers throughout the document and add them together.

-- For example:

--     [1,2,3] and {"a":2,"b":4} both have a sum of 6.
--     [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
--     {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
--     [] and {} both have a sum of 0.

-- You will not encounter any strings containing numbers.

-- What is the sum of all numbers in the document?

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Combinator
-- import Text.Parsec.Token
import System.Exit (exitFailure)
import Data.List.Split ( splitOn )
import Control.Monad
-- import Control.Applicative


data J = JGroup [J] 
       | JText String 
            deriving (Show, Eq)

jParser :: Parser J
jParser = jGroup

jGroup :: Parser J
jGroup = JGroup <$> many jItem

jItem :: Parser J
jItem =  try (between (char '{') (char '}') jGroup)
     <|> JText <$> many1 (noneOf "{}")
    

findNums :: String -> Int
findNums = sum .
           map read .
           filter (not . null) .
           splitOn "," .
           filter (\x -> x `elem` (',':'-':['0'..'9']))

main :: IO ()
main = do
    -- input <- readFile "./2015/day12/test"    
    input <- readFile "./2015/day12/day.json"
    dataSet <- case parse jParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x
    print $ head dataSet
    print "Part 1 Answer:"
    print $ findNums input