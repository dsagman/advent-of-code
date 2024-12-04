import Control.Monad.State

main :: IO ()
main = do
    inputLines <- readFile "2024/day4/day"
    -- inputLines <- readFile "2024/day4/test"
    let puz = lines inputLines
    -- let puz = a

    let right = puz
    let left  = map reverse puz
    let down  = transpose puz
    let up    = map reverse $ transpose puz
    let ll_ur = bDiag puz
    let ul_lr = bDiag $ transpose $ map reverse puz
    let lr_ul = bDiag $ map reverse puz
    let ur_ll = bDiag $ transpose puz

    let result = right ++ left ++ down ++ up ++ ll_ur ++ ul_lr ++ lr_ul ++ ur_ll
    print $ "Part 1 answer: " ++ show (sum $ map countXmas result)

-- Transpose a list of lists
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

-- https://stackoverflow.com/questions/32465776/getting-all-the-diagonals-of-a-matrix-in-haskell
bDiag :: [[a]] -> [[a]]
bDiag [] = []
bDiag ([]:xss) = xss
bDiag xss =  zipWith (++) (map ((:[]) . head) xss ++ repeat [])
                                  ([]:bDiag (map tail xss))

countXmas :: [Char] -> Int
countXmas xs = cnt where (_,cnt) = execState (xmasParser xs) (0,0)

-- Process the string character by character
xmasParser :: String -> State (Int, Int) ()
xmasParser [] = return ()
xmasParser (c:cs) = do
  xmasCharParser c
  xmasParser cs

xmasCharParser :: Char -> State (Int, Int) ()
xmasCharParser c = do
  (st, cnt) <- get
  case (st, c) of
    (_, 'X') -> put (1, cnt)
    (1, 'M') -> put (2, cnt)
    (2, 'A') -> put (3, cnt)
    (3, 'S') -> put (0, cnt + 1)
    _        -> put (0, cnt)