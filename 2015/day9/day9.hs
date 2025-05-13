-- For example, given the following distances:

-- London to Dublin = 464
-- London to Belfast = 518
-- Dublin to Belfast = 141

-- The possible routes are therefore:

-- Dublin -> London -> Belfast = 982
-- London -> Dublin -> Belfast = 605
-- London -> Belfast -> Dublin = 659
-- Dublin -> Belfast -> London = 659
-- Belfast -> Dublin -> London = 605
-- Belfast -> London -> Dublin = 982

-- The shortest of these is London -> Dublin -> Belfast = 605, and so the answer is 605 in this example.

-- What is the distance of the shortest route?

module Main where

import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)
import Data.List ( nub, permutations, sort )
import qualified Data.Map.Strict as Map
import Data.Maybe ( fromJust )

type CPair = (String, String)

distParser :: Parser (CPair, Int)
distParser = do
    c1 <- many1 letter
    string " to "
    c2 <- many1 letter
    string " = "
    d  <- read <$> many1 digit
    return ((c1,c2),d)

inputParser :: Parser [(CPair, Int)]
inputParser =  do
    many $ try (distParser <* newline)

pairToList :: CPair -> [String]
pairToList cp = [fst cp, snd cp]

listToPair :: [String] -> [CPair]
listToPair xs = zip xs (tail xs) 

cityPairs :: [String] -> [[CPair]]
cityPairs cities = map listToPair $ permutations cities

main :: IO ()
main = do
    -- input <- readFile "./2015/day12/test"    
    input <- readFile "./2015/day12/day.txt"
    dataSet <- case parse inputParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x
    let dataSetSwap = map (\((c1,c2),d) -> ((c2,c1),d)) dataSet
    let cMap = Map.fromList (dataSet <> dataSetSwap)

    let cities = nub $ concatMap (pairToList . fst) dataSet
    let routes = cityPairs cities
    let routeSums = sort $ map (sum . map (\ x -> fromJust $ Map.lookup x cMap)) routes
    print cities  -- the cities
    print $ length routes -- total permutations of routes

    print "Answer 1:"
    print $ head routeSums
    print "Answer 2:"
    print $ last routeSums
