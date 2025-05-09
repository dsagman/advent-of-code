
import Text.Parsec ( digit, letter, newline, space, string, choice, (<|>), many, many1, parse, try )
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)
import Data.Char ( toUpper )    
import Data.List ( sortOn )
import System.Process 


--- Data Structures

type Op = String
data Term = Const Int
           | Var String
           deriving (Show, Eq)

data Exp
  = Assign Term Term
  | NotOp Term Term
  | BinOp Op Term Term Term
  deriving (Show, Eq)

--- Parsers

termP :: Parser Term
termP =  Const . read <$> many1 digit
     <|> Var <$> many1 letter

assignP :: Parser Exp
assignP = do
    num <- termP
    string " -> "
    Assign num <$> termP

notP :: Parser Exp
notP = do
    string "NOT"
    space
    t1 <- termP
    string " -> "
    NotOp t1 <$> termP

binOpP :: Parser Exp
binOpP = do
    t1 <- termP
    space
    op <- many1 letter
    space
    t2 <- termP
    string " -> "
    BinOp op t1 t2 <$> termP

lineParser :: Parser Exp
lineParser = choice [try notP, try binOpP, try assignP]

inputParser :: Parser [Exp]
inputParser = many (try (lineParser <* newline))

--- Prolog Formatters 

-- Input ---
-- 123 -> x
-- x AND y -> d
-- x OR y -> e
-- x LSHIFT 2 -> f
-- y RSHIFT 2 -> g
-- NOT x -> h

-- Output ---
-- eval(x,123).
-- eval(a, A) :- eval(lx, A).
-- eval(d, D) :- eval(x, X), eval(y, Y), D is X /\ Y.
-- eval(e, E) :- eval(x, X), eval(y, Y), E is X \/ Y.
-- eval(f, F) :- eval(x, X), F is X << 2.
-- eval(g, G) :- eval(y, Y), G is Y >> 2.
-- eval(h, H) :- eval(x, X), H is ~X.

fmtT :: Term -> String
fmtT (Var x) = x
fmtT (Const x) = show x

fmtOp :: String -> String
fmtOp op = case op of
    "OR" -> "\\/"
    "AND" -> "/\\"
    "RSHIFT" -> ">>"
    "LSHIFT" -> "<<"
    _ -> "ERROR!!!!"

upT :: Term -> String
upT = map toUpper . fmtT

fmt :: Exp -> String
fmt (Assign t1@(Const x) t2) = 
    "eval(" ++ fmtT t2 ++ "," ++ fmtT t1 ++ ")."
fmt (Assign t1@(Var x) t2) = 
    "eval(" ++ fmtT t2 ++ "," ++ upT t2 ++ ") :- " ++
    "eval(" ++ fmtT t1 ++ "," ++ upT t2 ++ ")" ++ 
    "."
fmt (NotOp t1 t2) = 
    "eval(" ++ fmtT t2 ++ "," ++ upT t2 ++ ") :- " ++
    "eval(" ++ fmtT t1 ++ "," ++ upT t1 ++ "), " ++
    upT t2 ++ " is \\" ++ upT t1 ++ 
    "."
fmt (BinOp op t1 t2@(Const x) t3) = 
    "eval(" ++ fmtT t3 ++ "," ++ upT t3 ++ ") :- " ++
    "eval(" ++ fmtT t1 ++ "," ++ upT t1 ++ "), " ++
    upT t3 ++ " is " ++ 
    upT t1 ++ " " ++ fmtOp op ++ " " ++ upT t2 ++  
    "."
fmt (BinOp op t1@(Const x) t2 t3) = 
    "eval(" ++ fmtT t3 ++ "," ++ upT t3 ++ ") :- " ++
    "eval(" ++ fmtT t2 ++ "," ++ upT t2 ++ "), " ++
    upT t3 ++ " is " ++ 
    upT t1 ++ " " ++ fmtOp op ++ " " ++ upT t2 ++  
    "."
fmt (BinOp op t1 t2 t3) = 
    "eval(" ++ fmtT t3 ++ "," ++ upT t3 ++ ") :- " ++
    "eval(" ++ fmtT t1 ++ "," ++ upT t1 ++ "), " ++
    "eval(" ++ fmtT t2 ++ "," ++ upT t2 ++ "), " ++
    upT t3 ++ " is " ++ 
    upT t1 ++ " " ++ fmtOp op ++ " " ++ upT t2 ++  
    "."

-- Sorting function

lastTerm :: Exp -> Term
lastTerm (Assign _ t2)    = t2
lastTerm (NotOp  _ t2)     = t2
lastTerm (BinOp _ _ _ t3)  = t3

sortByLast :: [Exp] -> [Exp]
sortByLast = sortOn (\e ->
    let v = (fmtT . lastTerm) e
    in  (length v, v))

-- Prolog Generator

prologGen :: [Exp] -> String -> IO ()
prologGen dataset fn = do
    -- sorting is not strictly necessary.
    -- the key is tabling!
    let prologFile = fmt <$> sortByLast dataset
    -- mapM_ print prologFile -- just to see if it's working
    writeFile fn ":- initialization(main).\n"
    appendFile fn ":- table eval/2.\n"
    appendFile fn $ unlines prologFile
    appendFile fn "\n"
    appendFile fn "main :- \n"
    appendFile fn "     eval(a,A),\n"
    appendFile fn "     writeln(A)."

main :: IO ()
main = do
    -- input <- readFile "./2015/day7/test"
    input <- readFile "./2015/day7/day.txt"
    dataSet <- case parse inputParser "" input of
        Left err -> putStrLn ("Error parsing input: " ++ show err) >> exitFailure
        Right x  -> pure x

    let fn1 = "./2015/day7/day7part1.pl"
    let goal = "eval(a,A)"
    prologGen dataSet fn1
    part1 <- init <$> readProcess "swipl" ["-q", "-s", fn1, "-g", goal, "-t", "halt"] ""
    putStrLn "Answer Part 1:"
    putStrLn part1

    let fn2 = "./2015/day7/day7part2.pl"
    let sedCmd = "sed 's/^eval(b,[0-9]*).$/eval(b," ++ part1 ++ ")./' " 
                ++ fn1 ++ " > " ++ fn2
    callCommand sedCmd
    part2 <- init <$> readProcess "swipl" ["-q", "-s", fn2, "-g", goal, "-t", "halt"] ""
    putStrLn "Answer Part 2:"
    putStrLn part2

    




