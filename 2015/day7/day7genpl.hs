
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)
import Data.Char
import Data.List


type Op = String
data Term = Const Int
           | Var String
           deriving (Show, Eq)

data Exp
  = Assign Term Term
  | NotOp Term Term
  | BinOp Op Term Term Term
  deriving (Show, Eq)

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

-- 123 -> x
-- x AND y -> d
-- x OR y -> e
-- x LSHIFT 2 -> f
-- y RSHIFT 2 -> g
-- NOT x -> h

-- eval(x,123).
-- eval(a, A) :- eval(lx, A).
-- eval(d, D) :- eval(x, X), eval(y, Y), D is X /\ Y.
-- eval(e, E) :- eval(x, X), eval(y, Y), E is X \/ Y.
-- eval(f, F) :- eval(x, X), F is X << 2.
-- eval(g, G) :- eval(y, Y), G is Y >> 2.
-- eval(h, H) :- eval(x, X), H is ~X.

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

prologFmt :: [String] -> String
-- prologFmt xs = unlines $ map (("     " ++) . (++ ",")) xs
prologFmt = unlines

lastTerm :: Exp -> Term
lastTerm (Assign _ t2)    = t2
lastTerm (NotOp  _ t2)     = t2
lastTerm (BinOp _ _ _ t3)  = t3

sortByLast :: [Exp] -> [Exp]
sortByLast = sortOn (\e ->
    let v = (fmtT . lastTerm) e
    in  (length v, v)
  )

main :: IO ()
main = do
    -- input <- readFile "./2015/day7/test"
    input <- readFile "./2015/day7/day.txt"
    dataSet <- case parse inputParser "" input of
        Left err -> putStrLn ("Error parsing input: " ++ show err) >> exitFailure
        Right x  -> pure x

    -- sorting is not strictly necessary.
    -- the key is tabling!
    let prologFile = fmt <$> sortByLast dataSet
    mapM_ print prologFile
    let fn = "./2015/day7/day7gen.pl"
    writeFile fn ":- initialization(main).\n"
    appendFile fn ":- table eval/2.\n"
    appendFile fn $ prologFmt prologFile
    appendFile fn "\n"
    appendFile fn "main :- \n"
    appendFile fn "     eval(a,A),\n"
    appendFile fn "     writeln(A)."


