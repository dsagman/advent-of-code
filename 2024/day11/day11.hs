import qualified Data.Map as Map

main :: IO ()
main = do
        -- let datafile = "2024/day11/test"
        let datafile = "2024/day11/day.txt"
        input <- readFile datafile
        let stones = parse input
        let part1 =  (iterate . concatMap) blink stones !! 25
        print $ length part1
        let stonesCounter = foldr (\s -> Map.insertWith (+) s 1) Map.empty stones
        let part2 =  iterate blinkCounter stonesCounter !! 75
        print $ (sum . Map.elems) part2

blinkCounter :: Map.Map String Int -> Map.Map String Int
blinkCounter counter = Map.fromListWith (+) [ (newStone, count) | 
            (stone, count) <- Map.toList counter, 
            newStone <- blink stone]

blink :: String -> [String]
blink xs
  | read' xs == 0    = ["1"]
  | even $ length xs = [take half xs, show $ read'(drop half xs)]
  | otherwise        = [show $ read' xs * 2024]
    where half = length xs `div` 2
          read' :: String -> Int = read
  
parse :: String -> [String]
parse xs
  | null xs = []
  | otherwise = takeWhile (/= ' ') xs : parse (dropWhile (== ' ') (dropWhile (/= ' ') xs))

