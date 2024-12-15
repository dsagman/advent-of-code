
-- structure of input file
-- Button A: X+94, Y+34
-- Button B: X+22, Y+67
-- Prize: X=8400, Y=5400

parseButton :: String -> (String, Int, Int)
parseButton xs = (name, x, y)
    where
        name = takeWhile (/=':') $ drop 1 $ dropWhile (/=' ') xs
        x = read $ takeWhile (/=',') $ drop 2 $ dropWhile (/='X') xs
        y = read $ drop 2 $ dropWhile (/='Y') xs

parsePrize :: String -> (String, Int, Int)
parsePrize xs = ("P", x, y)
    where
        x = read $ takeWhile (/=',') $ drop 2 $ dropWhile (/='X') xs
        y = read $ drop 2 $ dropWhile (/='Y') xs

parseLine :: String -> (String, Int, Int)
parseLine xs
    | take 1 xs == "P" = parsePrize xs
    | take 1 xs == "B" = parseButton xs
    | otherwise = ("X", 0, 0)

groupButtons :: [a] -> [[a]]
groupButtons [] = []
groupButtons buttons = take 3 buttons : groupButtons (drop 3 buttons)


main :: IO ()
main = do
    -- let dataFile = "2024/day13/test"
    let dataFile = "2024/day13/day.txt"
    input <- readFile dataFile
    let buttons = groupButtons $ filter (/=("X",0,0)) $ map parseLine $ lines input
    -- mapM_ print buttons
    let part1sols = filter (/= []) $ map findAB buttons
    -- print $ part1sols
    let part1cost = sum $ (concatMap . map) costAB part1sols
    print $ part1cost

costAB :: (Int, Int) -> Int
costAB (a,b) = 3*a + b

findAB :: [(String, Int, Int)] -> [(Int, Int)]
findAB button = [(a,b) | a <- [0..100], b <- [0..100], a*xA + b*xB == xP, a*yA + b*yB == yP]
    where
        [aBut, bBut, prize] = button
        (nameA, xA, yA) = aBut
        (nameB, xB, yB) = bBut
        (nameP, xP, yP) = prize

