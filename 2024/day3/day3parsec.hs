{-# LANGUAGE LambdaCase #-}
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)

-- :set -package parsec

mulLineParser :: Parser [(Int, Int)]
mulLineParser =  many $ do
    _ <- manyTill anyChar (try (string "mul(") )
    mulParser

mulParser :: Parser ( Int,  Int)
mulParser = try (do
    firstNum <- read <$> many1 digit
    _ <- char ','
    secondNum <- read <$> many1 digit
    _ <- char ')'
    return ( firstNum,  secondNum))
    <|> return (0, 0)

-- mulDoParser :: Parser [(Int, Int)]
mulDoParser =  do
    _ <- manyTill anyChar (try (string "mul("))
    mulParser

doMulLineParser =  many $ do
    _ <- manyTill anyChar (try (lookAhead (string "do()")))
    doBlock <- between (string "do()") (string "don't()") (manyTill anyChar (try (lookAhead (string "don't()"))))
   
    case parse mulLineParser "" doBlock of
        Right x -> return x
        Left _ -> return [(0,0)]

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

    let modInput = filter (/= '\n') $ "do()" ++ input ++ "don't() do() HAH! don't()"
    -- print modInput
    nums2 <- case parse doMulLineParser "" modInput of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x

    print nums2
    -- mapM_ print nums2


    -- print $ "Part 2: " ++ show (length part2 + length part1)




