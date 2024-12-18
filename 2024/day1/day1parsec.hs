import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)
-- :set -package parsec

lineParser :: Parser (Int, Int)
lineParser = do
    num1 <- read <$> many1 digit  -- Parse the first number
    spaces                     -- Consume spaces
    num2 <- read <$> many1 digit  -- Parse the second number
    return (num1, num2)

inputParser :: Parser [(Int, Int)]
inputParser = do
    result <- lineParser `sepEndBy` newline  -- Parse multiple lines, separated by newlines
    eof -- Ensure whole file is consumed so that errors are thrown on bad input
    return result

main :: IO ()
main = do
    input <- readFile "2024/day1/day.txt"
    -- input <- readFile "2024/day1/test.txt"
    -- input <- readFile "2024/day1/error.txt"
    nums <- case parse inputParser "input" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x

    let sortedNums = map abs $ uncurry (zipWith (-)) $ unzip nums
    let (a, b) = unzip nums
    let countNums = zipWith (*) a (map (\x -> length $ filter (==x) b) a)

    putStr "Part 1 answer: "
    print $ sum sortedNums

    putStr "Part 2 answer: " 
    print $ sum countNums