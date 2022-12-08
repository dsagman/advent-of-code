{-# LANGUAGE DeriveFoldable #-}
module Main where
 
import Data.List 
import Data.Maybe


data Dir = D {name :: String, parent:: String, files :: [(String, Int)], dirs :: [Dir]}
        deriving (Show)

myDir = D { name = "root", parent = "", files = [("hello",234), ("bye", 123)], 
        dirs = [D {name = "a", parent = "root", files = [("one", 12), ("two",23)], 
                    dirs = [D {name = "b", parent ="a", files = [], dirs = []}]},
                D {name = "c", parent= "root", files =[("nextttt", 1200)], dirs = []}] 
        }


listAllDirs x = [name x] ++ ((concatMap) listAllDirs (dirs x)) 

findDir :: String -> Dir -> [Maybe Dir]
-- [Maybe Dir] because subdirectory name might not be unique
findDir x aDir 
    | x == name aDir = [Just aDir]
    | x `elem` (map name subDirs) = [(Just y) | y <- subDirs, x == name y] 
    | (not . null) subDirs  = concatMap (findDir x) subDirs
    | otherwise = [Nothing]
    where subDirs = dirs aDir
    


sumSize x = (sum . map snd) (files x) + sum (map sumSize (dirs x))
 

main :: IO ()
main = do
        -- dataFile <- readFile "day7/input.txt" 
        dataFile <- readFile "day7/test.txt" 
        let commands = lines dataFile
        -- print commands

        let a = parseAll (blankDir, [""]) commands
        print a
       
    
        let answer1 = "no can"
        let answer2 = "do"

        putStr "Part 1 answer: " 
        print answer1

        putStr "Part 2 answer: " 
        print answer2


-- parseAll :: (Dir, [[Char]]) -> (Dir, [String])
-- parseAll (x, []) = (x, [])
-- parseAll :: (Dir, [String], [String]) -> (Dir, [String], [String])
-- parseall :: (dir, [], curDir) = (dir, [], curDir)
-- parseAll (dir, cList, curDir) = parseAll (parse (dir, cList, curDir))



          


-- parseAll :: (Dir, [String]) -> [String] -> (Dir, [String])
-- parseall (dir, curDir) [] = parse (dir, curDir) ""
-- parseall (dir, curDir) [c] = parse (dir, curDir) c
-- parseAll (dir, curDir) (c:cList) = parseAll (parse (dir, curDir) c) cList
parseAll (dir, curDir) cList = [parse (dir, curDir) c | c <- cList]

parse :: (Dir, [String]) -> [Char] -> (Dir, [String])
parse (dir, curDir) "" = (dir, curDir)
parse (dir, curDir) command 
        | take 4 command == "$ cd" = interpretCD dir curDir command
        | take 4 command == "$ ls" = interpretLS dir curDir command
        | take 3 command == "dir"  = interpretDIR dir curDir command

        | otherwise =  (dir, curDir)  

-- interpretDIR dir curDir command = 
--     where token = drop 4 command

interpretCD :: Dir -> [String] -> [Char] -> (Dir, [String])
interpretCD dir curDir command
    | token == "/" = ((addName "/" dir), ["/"])
    | token == ".." = (dir, init curDir)
    | otherwise = (dir, curDir++[token]) 
    where token = drop 5 command
           

interpretLS dir curDir command = (dir, curDir)


addFile newF aDir = aDir {files = newF:(files aDir)}

-- addDir newD aDir = aDir {dirs = myNewD:(dirs aDir)}
--     where myNewD = newD {parent = name aDir}

addDir newN curDir = 

addName newN aDir = aDir {name = newN}

newDir newN = addName newN blankDir

blankDir = D {name = "", parent = "", files = [], dirs = []}
