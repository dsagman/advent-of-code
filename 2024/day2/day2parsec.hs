import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)

-- :set -package parsec

lineParser :: Parser [Int]
lineParser = do
    nums <- many1 digit `sepBy` char ' '
    return $ map read nums

inputParser :: Parser [[Int]]
inputParser = do
    result <- lineParser `sepEndBy` newline  -- Parse multiple lines, separated by newlines
    eof -- Ensure whole file is consumed so that errors are thrown on bad input
    return $ filter (/= []) result

main :: IO ()
main = do
    input <- readFile "2024/day2/day.txt"
    -- input <- readFile "2024/day2/test.txt"
    nums <- case parse inputParser "input" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x

    -- let part1 = [x | x <- nums, safe $ deltas x]
    -- let unSafe = [x | x <- nums, not $ safe $ deltas x]
    -- let part2 = [x | x <- unSafe, any (safe . deltas) (dropEach x)]
    let part1 = filter (safe . deltas) nums
    let unSafe = filter (not . safe . deltas) nums
    let part2 = filter (any (safe . deltas) . dropEach) unSafe
 
    print $ "Part 1: " ++ show (length part1)
    print $ "Part 2: " ++ show (length part2 + length part1)


deltas :: [Int] -> [Int]
deltas x = zipWith (-) x (tail x)

safe :: [Int] -> Bool
safe xs = (all (>0) xs && all (<=3) xs) || (all (<0) xs && all (>=(-3)) xs)

dropEach :: [a] -> [[a]]
dropEach xs = [take n xs ++ drop (n+1) xs | n <- [0..length xs - 1]]

