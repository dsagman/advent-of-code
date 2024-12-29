import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)
import Data.Maybe (fromJust, isJust, isNothing, fromMaybe)
import Data.List (sortBy)
import qualified Data.Bifunctor

type Logic = (String, String, String, String)
type Gate = (String, Int)
type GateMaybe = (String, Maybe Int)

inputParser :: Parser ([Gate], [Logic])
inputParser =  do
    xyGates <- many $ try (xyGateParser <* newline)
    newline
    gates <- many $ try (gateParser <* newline)
    return (xyGates, gates)

-- x00: 1
xyGateParser :: Parser Gate
xyGateParser = do
    gate <- many1 alphaNum
    string ": "
    value <- read <$> many1 digit
    return (gate, value)

-- ntg XOR fgs -> mjb
gateParser :: Parser Logic
gateParser = do
    gateA <- many1 alphaNum
    char ' '
    logic <- many1 alphaNum
    char ' '
    gateB <- many1 alphaNum
    string " -> "
    gateC <- many1 alphaNum
    return (gateA, logic, gateB, gateC)

main :: IO ()
main = do
    input <- readFile "2024/day24/day.txt"
    -- input <- readFile "2024/day24/test"
    dataSet <- case parse inputParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x

    let (xyGates, logic) = dataSet
    let allGates = concatMap initGates logic
    let startGates = [mergeGate a (lookup (fst a) xyGates) | a <- allGates]

    let evals = map (Data.Bifunctor.second fromJust) (evalGates logic startGates)
    let zEvals = filter (\(g, v) -> head g == 'z') evals
    print $ listToBinary $ map snd $ sortBy (flip (\(a,_) (b,_) -> compare a b)) zEvals

listToBinary :: [Int] -> Int
listToBinary = foldl (\acc x -> 2*acc + x) 0

initGates :: Logic -> [GateMaybe]
initGates (a, _, b, c) = [(a, Nothing), (b, Nothing), (c, Nothing)]

mergeGate :: GateMaybe -> Maybe Int -> GateMaybe
mergeGate (a, aState) bState
    | isJust aState = (a, aState)
    | isJust bState = (a, bState)
    | otherwise     = (a, Nothing)

evalGates :: [Logic] -> [GateMaybe] -> [GateMaybe]
evalGates logic gates = do
    if all (\(g, v) -> isJust v) gates
    then gates
    else do
        let gates' = [(c, fromJust v) | (c, v) <- gates, isJust v]
        let evals = [evalGate g gates' | g <- logic]
        let evals' = [(c, fromJust v) | (c, v) <- evals, isJust v]
        let mergedGates = [mergeGate a (lookup (fst a) evals') | a <- gates]
        evalGates logic mergedGates

evalGate :: Logic -> [Gate] -> GateMaybe
evalGate (a,g,b,c) gates =
    case g of
        "AND" -> (c, intMaybeAnd valueA valueB)
        "OR"  -> (c, intMaybeOr  valueA valueB)
        "XOR" -> (c, intMaybeXor valueA valueB)
    where
        valueA = lookup a gates
        valueB = lookup b gates

intMaybeAnd:: Maybe Int -> Maybe Int -> Maybe Int
intMaybeAnd x y
  | isNothing x || isNothing y = Nothing
  | x == Just 1 && y == Just 1 = Just 1
  | otherwise = Just 0


intMaybeOr:: Maybe Int -> Maybe Int -> Maybe Int
intMaybeOr x y
  | isNothing x || isNothing y = Nothing
  | x == Just 1 || y == Just 1 = Just 1
  | otherwise                  = Just 0

intMaybeXor:: Maybe Int -> Maybe Int -> Maybe Int
intMaybeXor x y
  | isNothing x || isNothing y = Nothing
  | x == y    = Just 0
  | otherwise = Just 1
