
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)


type Op = String
data Term = Const Int
           | Var String
           deriving Show

data Exp
  = AssignOp Op Term Term
  | NotOp Op Term Term
  | BinOp Op Term Term Term
  deriving Show

termP :: Parser Term
termP =  Const . read <$> many1 digit
     <|> Var <$> many1 letter

assignP :: Parser Exp
assignP = do
    num <- termP
    string " -> "
    AssignOp ":=" num <$> termP

notP :: Parser Exp
notP = do
    op <- string "NOT"
    space
    t1 <- termP
    string " -> "
    NotOp op t1 <$> termP

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

-- inputParser :: Parser [Exp]
-- inputParser = endBy lineParser newline

main :: IO ()
main = do
    input <- readFile "./2015/day7/day.txt"
    dataSet <- case parse inputParser "" input of
        Left err -> putStrLn ("Error parsing input: " ++ show err) >> exitFailure
        Right x  -> pure x

    print dataSet
    print $ length dataSet

