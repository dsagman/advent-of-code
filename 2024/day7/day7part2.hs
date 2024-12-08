import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)

inputParser :: Parser [(Int, [[Char]])]
inputParser = lineParser `sepBy` endOfLine <* eof

--example line is 190: 10 19
lineParser :: Parser (Int, [[Char]])
lineParser = do
    c <- read <$> many1 digit
    string ": "
    coeffs <- intStringParser
    return (c, coeffs)

intStringParser :: Parser [[Char]]
intStringParser = many1 digit `sepBy` char ' '

main :: IO ()
main = do
    input <- readFile "2024/day7/test"
    -- input <- readFile "2024/day7/day.txt"
    dataSet <- case parse inputParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x
    print dataSet
    print $ map evalExpr dataSet
    -- let evals = filter (uncurry elem) (map evalExpr dataSet)
--     print $ "Part 1: " ++ show (sum $ map fst evals)

-- addMul :: [Int] -> [Int]
-- addMul [x] = [x]
-- addMul xs = do
--     combination <- addMul (tail xs)
--     [head xs + combination, head xs * combination]

-- reverse so that we evaluate left to right
evalExpr :: (a, [[Char]]) -> (a, [[Char]])
evalExpr (c, coeffs) = (c, addMul (reverse coeffs))

addMul :: [[Char]] -> [[Char]]
addMul [x] = [x]
addMul xs = do
    combination <- addMul (tail xs)
    [head xs ++ "+" ++ combination, 
     head xs ++ "*" ++ combination,
     head xs ++ "||" ++ combination]

-- def parse_num(xs: str) -> int:
--     ''' Number parser consumes string '''
--     i = 0
--     while i < len(xs) and xs[i].isdigit():
--         i += 1
--     return (int(xs[:i]), xs[i:])

-- def eval_expr(expr: str) -> int:
--     ''' Expression string evaluator left to right'''
--     x, expr = parse_num(expr)
--     if not expr:
--         return x
--     # y = eval_expr(expr[1:]) # recursion
--     op = expr[0]
--     if op == "+": return x + eval_expr(expr[1:]) 
--     if op == "*": return x * eval_expr(expr[1:]) 