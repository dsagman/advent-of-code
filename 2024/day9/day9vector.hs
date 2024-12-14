import qualified Data.Vector as V

main :: IO ()
main =
    do
        -- let datafile = "2024/day9/test"
        let datafile = "2024/day9/day.txt"
        input <- readFile datafile
        let mem :: V.Vector Int = (expand . head . lines) input
        let free = V.findIndices (== -1) mem
        let final =  compress mem free
        print $ checksum final


compress :: V.Vector Int -> V.Vector Int -> V.Vector Int
compress mem freeSpace 
    | V.null freeSpace = mem -- Terminate when no free space is left
    | otherwise =
        if lb == -1 then 
            compress updMem $ V.init freeSpace -- drop if lb is free space
        else 
            compress newMem (V.tail freeSpace) 
        where lb = V.last mem
              updMem = V.init mem
              idx = V.head freeSpace
              newMem = V.update updMem $ V.fromList [(idx, lb)]


expand :: String -> V.Vector Int
expand xs = V.concat [V.replicate (read [c] :: Int) (f i) | (i, c) <- zip [0..] xs]
    where f i = if even i then i `div` 2 else -1

checksum :: V.Vector Int -> Int
checksum xs = V.sum $ V.imap (\i c -> if c == -1 then 0 else i * c) xs

