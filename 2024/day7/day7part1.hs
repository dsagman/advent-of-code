import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)

inputParser :: Parser [(Int, [Int])]
inputParser = lineParser `sepBy` endOfLine <* eof

--example line is 190: 10 19
lineParser :: Parser (Int, [Int])
lineParser = do
    c <- read <$> many1 digit
    string ": "
    coeffs <- intListParser
    return (c, coeffs)

intListParser :: Parser [Int]
intListParser = (read <$> many1 digit) `sepBy` char ' '

main :: IO ()
main = do
    -- input <- readFile "2024/day7/test"
    input <- readFile "2024/day7/day.txt"
    dataSet <- case parse inputParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x

    let evals = filter (uncurry elem) (map evalExpr dataSet)
    print $ "Part 1: " ++ show (sum $ map fst evals)

addMul :: [Int] -> [Int]
addMul [x] = [x]
addMul xs = do
    combination <- addMul (tail xs)
    [head xs + combination, head xs * combination]

-- reverse so that we evaluate left to right
evalExpr :: (a, [Int]) -> (a, [Int])
evalExpr (c, coeffs) = (c, addMul (reverse coeffs))

