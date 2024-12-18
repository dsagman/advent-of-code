
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
    let part1sols = filter (/= []) $ map findAB buttons
    let part1cost = sum $ (concatMap . map) costAB part1sols
    print $ part1cost
    -- mapM_ (print . part2prize) buttons
    let buttons2 = map part2prize buttons
    let part2sols = map solveAB buttons
    let part2cost = sum $ map costAB part2sols
    print $ part2cost

part2prize ::[(String, Int, Int)] -> [(String, Int, Int)]
part2prize button = [aBut, bBut, (nameP, xP+10000000000000, yP+10000000000000)]
    where
        [aBut, bBut, prize] = button
        (nameA, xA, yA) = aBut
        (nameB, xB, yB) = bBut
        (nameP, xP, yP) = prize

costAB :: (Int, Int) -> Int
costAB (a,b) = 3*a + b

findAB :: [(String, Int, Int)] -> [(Int, Int)]
findAB button = [(a,b) | a <- [0..100], b <- [0..100], a*xA + b*xB == xP, a*yA + b*yB == yP]
    where
        [aBut, bBut, prize] = button
        (nameA, xA, yA) = aBut
        (nameB, xB, yB) = bBut
        (nameP, xP, yP) = prize

-- b=(py*ax-px*ay)/(by*ax-bx*ay) a=(px-b*bx)/ax        
solveAB :: [(String, Int, Int)] -> (Int, Int)
solveAB button = (a,b)
    where
        b = (yP*xA - xP*yA) `div` (yB*xA - xB*yA)
        a = (xP - b*xB) `div` xA
        [aBut, bBut, prize] = button
        (nameA, xA, yA) = aBut
        (nameB, xB, yB) = bBut
        (nameP, xP, yP) = prize