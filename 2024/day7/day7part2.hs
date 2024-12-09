import Text.Parsec
    ( char, digit, endOfLine, string, eof, sepBy, many1, parse )
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)
import Control.Monad.State ( MonadState(put, get), State, evalState, runState, execState )
import Data.Bifunctor (second)
import Data.Char (isDigit)
import System.Console.ANSI (xtermSystem)

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
    -- input <- readFile "2024/day7/test"
    input <- readFile "2024/day7/day.txt"
    dataSet <- case parse inputParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x
    -- print dataSet
    -- let exprs = map (\(c, coeffs) -> (c, addMul (reverse coeffs))) dataSet
    let exprs = map (Data.Bifunctor.second addMul) dataSet
    -- print $ exprs !! 4
    let evals = map (Data.Bifunctor.second (map evalExpr)) exprs
    -- print $ evals !! 4
    let okEvals = filter (uncurry elem) evals
    print $ map fst okEvals
    print $ "Part 2: " ++ show (sum $ map fst okEvals)

addMul :: [[Char]] -> [[Char]]
addMul [x] = [x]
addMul xs = do
    combination <- addMul (tail xs)
    [head xs ++ "+" ++ combination,
     head xs ++ "*" ++ combination,
     head xs ++ "||" ++ combination]


-- Parse digits one by one to build an integer
parseNum :: State String String
parseNum = go ""
  where
    go acc = do
        xs <- get
        case xs of
            [] -> return acc -- End of input, return the accumulated number
            (x:rest) 
                | isDigit x -> do
                    put rest -- Consume the character
                    go (acc ++ [x]) -- Add the digit to the accumulator
                | otherwise -> return acc -- Stop parsing and return the accumulated number

parseOps :: State String [String]
parseOps = go []
  where
    go acc = do
        xs <- get
        case xs of
            [] -> return (reverse acc) -- Return the accumulated list when input is exhausted
            (x:rest)
                | isDigit x -> do
                    let (num, remaining) = runState parseNum xs
                    put remaining
                    go (num : acc) -- Add the parsed number to the accumulator
                | x == '|' && not (null rest) && head rest == '|' -> do
                    put (tail rest) -- Skip the second '|'
                    go ("||" : acc) -- Add "||" as a token
                | x `elem` "+*" -> do
                    put rest
                    go ([x] : acc) -- Add the operator to the accumulator
                | otherwise -> error $ "Invalid character: " ++ [x]

evalExpr :: String -> Int
evalExpr xs = do 
    go (evalState parseOps xs) 
    where
        go xs = do
            case xs of
                [] -> 0
                [x] -> read x
                (x:op:y:rest) -> case op of
                    "+" -> go (show (read x + read y) : rest)
                    "*" -> go (show (read x * read y) : rest)
                    "||" -> go (show (read (x ++ y) :: Int) : rest)
                    _ -> error "Invalid operator"
    

