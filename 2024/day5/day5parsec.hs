import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)

inputParser :: Parser ([(Int, Int)], [[Int]])
inputParser =  do
    pairs <- many $ try (pairParser <* newline)
    newline
    nums <- intListParser `sepBy` newline
    eof
    return (pairs, nums)

pairParser :: Parser (Int, Int)
pairParser = do
    num1 <- read <$> many1 digit
    char '|'
    num2 <- read <$> many1 digit
    return (num1, num2)

intListParser :: Parser [Int]
intListParser = do
    (read <$> many1 digit) `sepBy` char ','

main :: IO ()
main = do
    input <- readFile "2024/day5/day.txt"
    -- input <- readFile "2024/day5/test"
    dataSet <- case parse inputParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x
    
    let (rules, nums) = dataSet
    let okNums = [xs | xs <- nums, okOrder rules xs]
    print $ "Part 1 answer: " ++ show (sum $ map middle okNums)

    let notOkNums = [xs | xs <- nums, not $ okOrder rules xs]
    let sortedNotOK = [qSortRules rules xs | xs <- notOkNums]
    print $ "Part 2 answer: " ++ show (sum $ map middle sortedNotOK)

pairWise :: [b] -> [(b, b)]
pairWise xs = zip xs (tail xs)

okPair :: [(Int, Int)] -> (Int, Int) -> Bool
okPair rules (x, y) = (x, y) `elem` rules

okOrder ::  [(Int, Int)] -> [Int] -> Bool
okOrder rules nums = and [okPair rules pair | pair <- pairWise nums]

middle :: [a] -> a
middle xs = xs !! (length xs `div` 2)

qSortRules :: [(Int, Int)] -> [Int] -> [Int]
qSortRules rules []     = []
qSortRules rules (p:xs) = qSortRules rules lesser ++ [p] ++ qSortRules rules greater
    where
        lesser  = filter (\x -> okPair rules (x,p) ) xs
        greater = filter (\x -> not $ okPair rules (x,p) ) xs