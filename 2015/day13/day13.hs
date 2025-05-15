import Text.Parsec.String
import System.Exit
import Text.Parsec
import qualified Data.HashMap.Strict as Map
import Data.List
import Data.Maybe


lineParser :: Parser ((String, String), Int)
lineParser = do
    name1 <- many letter
    string " would "
    delta <- 1 <$ string "gain " <|> (-1) <$ string "lose "
    n <- read <$> many digit
    skipMany (lower <|> space)
    name2 <- many letter
    char '.'
    pure ((name1, name2), n * delta)

inputParser :: Parser [((String, String), Int)]
inputParser = do
    many $ try (lineParser <* newline)

seatVal :: [String] -> Map.HashMap (String, String) Int -> Int
seatVal xs m = 
    f seatR + f seatL
    where 
        seatR =  xs ++ [head xs]
        seatL =  reverse seatR
        f ys = sum $ zipWith (curry (fromJust . (`Map.lookup` m))) ys (tail ys)


seatings :: [String] -> Map.HashMap (String, String) Int -> [Int]
seatings xs m = map (`seatVal` m) $ permutations xs

main :: IO ()
main = do
    -- input <- readFile "./2015/day13/test"
    input <- readFile "./2015/day13/day.txt"
    dataSet <- case parse inputParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x

    let people = nub [x | ((x,_),_) <- dataSet]
    let pMap = Map.fromList dataSet
    let pSeating = seatings people pMap
    print "Part 1 Answer:"
    print $ "Choices " ++ show (length pSeating)
    print $ maximum pSeating
    
    let mePeople = "me":people
    let meMap = Map.union pMap $ 
                Map.fromList ([(("me",x), 0) | x <- people] ++
                              [((x,"me"), 0) | x <- people]) 
    let meSeating = seatings mePeople meMap
    print "Part 2 Answer:"
    print $ "Choices " ++ show (length meSeating)
    print $ maximum meSeating