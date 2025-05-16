import Data.List

data Deer = Deer {name :: String,
                  kms :: Int,
                  fly :: Int,
                  rest :: Int}
        deriving (Show, Eq)

parse :: [String] -> Deer
parse (name:_:_:kms:_:_:fly:_:_:_:_:_:_:rest:_) =
        Deer { name = name,
               kms  = read kms,
               fly  = read fly,
               rest = read rest }

-- we don't need dist, we could just use incD for part 1 and 2
dist :: Deer -> Int -> Int
dist d t = let jumpTime = fly d + rest d
           in -- jump dist * whole jumps
           (kms d * fly d) * (t `div` jumpTime)  +
            -- partial jump      
           (kms d * min (t `mod` jumpTime) (fly d))

incD :: Deer -> Int -> [Int]
incD d n = drop 1 $ take (n+1) $ scanl f 0 (zip [0..] (repeat d) )
        where f z x = z + 
                if fst x `mod` (fly d + rest d) < fly d   
                then kms d else 0

tagMax :: [[Int]] -> [[Int]]
tagMax = transpose . map mask . transpose
    where mask xs = map (\x -> if x == maximum xs then 1 else 0 ) xs
    
main :: IO ()
main = do
    -- input <- lines <$> readFile "./2015/day14/test"
    input <- lines <$> readFile "./2015/day14/day.txt"
    let ds = map (parse . words) input
    mapM_ print $ take 3 ds
    let dists = map (`dist` 2503) ds
    print "Answer Part 1:"
    print $ maximum dists

    print "Answer Part 2:"
    let distSecs = map (`incD` 2503) ds
    let points = map sum $ tagMax distSecs
    print $ maximum points


-- recursive version of incD

incD' :: Deer -> Int -> Int -> Int -> [Int]
incD' d curD n maxN 
    | n == maxN  = [curD] 
    | t < fly d = curD : incD' d (curD + kms d) (n+1) maxN
    | otherwise  = curD : incD' d curD (n+1) maxN
    where 
        t = n `mod` (fly d + rest d)