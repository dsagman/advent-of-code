-- For example, imagine a simpler machine that supports only the following replacements:

-- H => HO
-- H => OH
-- O => HH

-- Given the replacements above and starting with HOH, the following molecules could be generated:

--     HOOH (via H => HO on the first H).
--     HOHO (via H => HO on the second H).
--     OHOH (via H => OH on the first H).
--     HOOH (via H => OH on the second H).
--     HHHH (via O => HH).

-- So, in the example above, there are 4 distinct molecules (not five, because HOOH appears twice) after one replacement from HOH. Santa's favorite molecule, HOHOHO, can become 7 distinct molecules (over nine replacements: six from H, and three from O).

-- The machine replaces without regard for the surrounding characters. For example, given the string H2O, the transition H => OO would result in OO2O.

-- Your puzzle input describes all of the possible replacements and, at the bottom, the medicine molecule for which you need to calibrate the machine. How many distinct molecules can be created after all the different ways you can do one replacement on the medicine molecule?
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)
import Data.List

swapParser :: Parser (String, String)
swapParser = do
    x <- many1 letter
    string " => "
    y <- many1 letter
    pure (x, y)

dataParser :: Parser String
dataParser = many1 letter

inputParser :: Parser ([(String, String)], String)
inputParser = do
    swaps <- many (try (swapParser <* newline))
    newline
    d <- dataParser
    return (swaps, d)

swap :: String -> String -> String -> [String]
swap x y xs = go x y xs "" []

go :: String -> String -> String -> String -> [String] -> [String]
go _ _ []         _    acc = acc   
go x y zss@(z:zs) left acc 
    | x1 == x   = go x y x2 (left++x1) (acc ++ [left++y++x2])
    | otherwise = go x y zs (left++[z]) acc
    where l = length x
          x1 = take l zss
          x2 = drop l zss 

main :: IO ()
main = do 
    -- input <- readFile "./2015/day19/test"
    -- input <- readFile "./2015/day19/test2"
    input <- readFile "./2015/day19/day.txt"

    dataSet <- case parse inputParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x

    let part1 = (concatMap (\x -> uncurry swap x (snd dataSet)) . fst) dataSet
    print "Answer Part 1:"
    print $ length $ nub part1



    -- print $ splitOn (fst t1) input
    -- print $ uncurry swap t0 "HOH"
    -- print $ swap "H" "OH" "HOH"