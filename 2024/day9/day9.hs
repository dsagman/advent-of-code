-- So slow because of ++

main :: IO ()
main =
    do
        -- let datafile = "2024/day9/test"
        let datafile = "2024/day9/day.txt"
        input <- readFile datafile
        let mem = (expand . head . lines) input
        let free :: [Int] = [i | (i, c) <- zip [0..] mem, c == -1]
        let final =  compress mem free
        print $ checksum final

compress :: [Int] -> [Int] -> [Int]
compress mem freeSpace 
    | null freeSpace = mem -- Terminate when no free space is left
    | otherwise =
        if lb == -1 then 
            compress updMem $ init freeSpace -- drop if lb is free space
        else 
            compress newMem (tail freeSpace) 
        where lb = last mem
              updMem = init mem
              idx = head freeSpace
              newMem = take idx updMem ++ [lb] ++ drop (idx + 1) updMem


expand :: String -> [Int]
expand xs = concat [replicate (read [c] :: Int) (f i) | (i, c) <- zip [0..] xs]
    where f i = if even i then i `div` 2 else -1

checksum :: [Int] -> Int
checksum xs = sum [i*c | (i, c) <- zip [0..] xs, c /= -1]

