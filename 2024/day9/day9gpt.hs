import Control.Monad.State
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST

type Memory = MV.MVector s Int -- Mutable vector for memory
type FreeSpace = [Int]         -- List of indices representing free space
type CompressState s = (Memory s, FreeSpace)

-- Compress function
compress :: StateT (CompressState s) (ST s) ()
compress = do
    (mem, freeSpace) <- get
    case freeSpace of
        [] -> return () -- Terminate when no free space is left
        (idx:fs) -> do
            size <- MV.length mem
            lb <- MV.read mem (size - 1) -- Read the last element
            MV.write mem (size - 1) (-1) -- Mark the last slot as free
            if lb == -1
                then put (mem, idx:init fs) -- Drop if last block is free space
                else do
                    MV.write mem idx lb -- Move the block to the free space index
                    put (mem, fs)
            compress

main :: IO ()
main = do
    let input = "221331" -- Example input
    let mem = expand input
    let freeSpace = [i | (i, c) <- zip [0..] mem, c == -1]
    let finalMem = runST $ do
            memVec <- MV.new (length mem)
            mapM_ (uncurry $ MV.write memVec) (zip [0..] mem)
            execStateT compress (memVec, freeSpace)
            mapM (MV.read memVec) [0..length mem - 1]
    print $ checksum finalMem

expand :: String -> [Int]
expand xs = concat [replicate (read [c] :: Int) (f i) | (i, c) <- zip [0..] xs]
    where f i = if even i then i `div` 2 else -1

checksum :: [Int] -> Int
checksum xs = sum [i * c | (i, c) <- zip [0..] xs, c /= -1]
