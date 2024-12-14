import Control.Monad.State
import Data.Maybe

main :: IO ()
main =
    do
        let datafile = "2024/day9/test"
        -- let datafile = "2024/day9/day.txt"
        input <- readFile datafile
        let mem = (expand . head . lines) input
        let free :: [Int] = [i | (i, c) <- zip [0..] mem, c == -1]
        let (final, _) = execState compress (mem, free)
        print $ checksum final

compress :: State ([Int], [Int]) ()
compress = do
    (mem, freeSpace) <- get
    case freeSpace of
        [] -> return () -- Terminate when no free space is left
        (idx:fs) -> do
            let lb :: Int = last mem
            let updMem :: [Int]= init mem
            if lb == -1 then do
                put (updMem, idx:init fs) -- drop if lb is free space
            else do
                let newMem = take idx updMem ++ [lb] ++ drop (idx + 1) updMem
                put (newMem, fs)
            compress

expand :: String -> [Int]
expand xs = concat [replicate (read [c] :: Int) (f i) | (i, c) <- zip [0..] xs]
    where f i = if even i then i `div` 2 else -1

checksum :: [Int] -> Int
checksum xs = sum [i*c | (i, c) <- zip [0..] xs, c /= -1]

