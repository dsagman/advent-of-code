module Main where
 
import Data.List ( sort ) 
import Data.List.Split ( splitOn )
import qualified Data.Map as Map

main :: IO ()
main = do
        dataFile <- readFile "day7/input.txt" 
        -- dataFile <- readFile "day7/test.txt" 
        let commands = lines dataFile

        let theDirs = fst $ parseAll commands (blankDir, [])
        let answer1 =  (sum . Map.elems . Map.filter (<= 100000) . sumDirs) theDirs
       
        let diskSize = 70000000 
        let usedSpace = (sum . Map.elems . Map.map (sum . map fst)) theDirs
        let needSpace = 30000000 - (diskSize - usedSpace)

        let answer2 = (head . sort . Map.elems . Map.filter (>= needSpace) . sumDirs) theDirs

        putStr "Part 1 answer: " 
        print answer1

        putStr "Part 2 answer: " 
        print answer2


type Dir = Map.Map [String] [(Int, String)]

sumDirs :: Dir -> Map.Map [String] Int
sumDirs dir = Map.fromList [(k, (sum . concat) (Map.elems (Map.map (map fst) 
                   (Map.filterWithKey (\k' _ -> (subDir k k')) dir)) ) )
                   | k <- Map.keys dir]

subDir :: Eq a => [a] -> [a] -> Bool
subDir a b = a == (take (length a)) b

blankDir :: Dir
blankDir = Map.fromList []

parseAll :: [String]  -> (Dir, [String]) -> (Dir, [String])
parseAll (c:cList) (dir, curDir)  
        | null cList = parse c (dir, curDir)
        | otherwise = parseAll cList $ parse c (dir, curDir)

parse :: String  -> (Dir, [String]) -> (Dir, [String])
parse command (pathMap, curDir) 
        | take 4 command == "$ cd" = interpretCD command (pathMap, curDir)
        | take 4 command == "$ ls" = (pathMap, curDir)
        | take 3 command == "dir"  = interpretDir command (pathMap, curDir)
        | otherwise = interpretFILE command (pathMap, curDir)
       

interpretDir :: String  -> (Dir, [String]) -> (Dir, [String])
interpretDir command (pathMap, curDir) = (Map.insert (curDir++[token]) [] pathMap, curDir)
        where token = drop 4 command

interpretCD :: String  -> (Dir, [String]) -> (Dir, [String])
interpretCD command (pathMap, curDir) 
    | token == "/" = (pathMap, ["/"]) 
    | token == ".." = (pathMap, init curDir)
    | otherwise = (pathMap, curDir++[token])
    where token = drop 5 command
           
interpretFILE :: String  -> (Dir, [String]) -> (Dir, [String])
interpretFILE lexeme (pathMap, curDir) = (Map.insertWith (++) curDir [(fSize, fName)] pathMap, curDir)
        where [fSizeString, fName] = splitOn " " lexeme
              fSize = read fSizeString :: Int
        

