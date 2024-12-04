import Distribution.Compat.Graph (neighbors)
import Data.List (sort)
main :: IO ()
main = do
    inputLines <- readFile "2024/day4/day"
    -- inputLines <- readFile "2024/day4/test"
    let puz = lines inputLines
    let w = length $ head puz
    let h = length puz
    let aLocs = getAs $ zipIndexes puz
    let aBors = map (\((x,y),c) -> dBors w h x y) aLocs
    let x_as = (map . map) (\ (x, y) -> puz !! y !! x) aBors
    let x_mas = [xs | xs <- x_as, xs `elem` ["MSMS", "SMSM", "SSMM", "MMSS"]]

    print $ "Part 2 answer: " ++ show (length x_mas)

dBors :: Int -> Int -> Int -> Int -> [(Int, Int)]
dBors width height x y = [(x', y') |
                            x' <- [x - 1, x + 1],
                            x' >= 0,
                            x' < width,
                            y' <- [y - 1, y + 1],
                            y' >= 0,
                            y' < height]

zipIndexes :: [[a]] -> [[((Int, Int), a)]]
zipIndexes = zipWith (\y xs -> zipWith (\x a -> ((x, y), a)) [0..] xs) [0..]

getAs :: Foldable t => t [((a, b), Char)] -> [((a, b), Char)]
getAs puz = [((x, y), c) | ((x, y), c) <- concat puz, c == 'A']