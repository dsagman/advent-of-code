import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)

-- :set -package parsec

mulLineParser :: Parser [(Int, Int)]
mulLineParser =  many $ try $ do
    _ <- manyTill anyChar (try (string "mul("))
    mulParser
    
mulParser :: Parser ( Int,  Int)
mulParser =  try (do
    firstNum <- read <$> many1 digit
    _ <- char ','
    secondNum <- read <$> many1 digit
    _ <- char ')'
    return ( firstNum,  secondNum))
    <|> return (0, 0)

doParser :: Parser [[(Int,Int)]]
doParser =  many $ try $ do
    _ <- manyTill anyChar (try (lookAhead (string "do()")))
    doBlock <- between (string "do()") (string "don't()") (manyTill anyChar (try (lookAhead (string "don't()"))))
    let muls = runParser mulLineParser () "" doBlock
    case muls of
        Left _ -> return [(0, 0)]
        Right x -> return x

main :: IO () 
main = do
    input <- readFile "2024/day3/day.txt"
    -- input <- readFile "2024/day3/test.txt"
    nums1 <- case parse mulLineParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x

    let ans1 = sum (map (uncurry (*)) nums1)
    print $ "Part 1: " ++ show ans1

    let modInput = "do()" ++ input 
    nums2 <- case parse doParser "" modInput of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x

    let ans2 = sum (map (uncurry (*)) (concat nums2))
    print $ "Part 2: " ++ show ans2





