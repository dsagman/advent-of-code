-- children: 3
-- cats: 7
-- samoyeds: 2
-- pomeranians: 3
-- akitas: 0
-- vizslas: 0
-- goldfish: 5
-- trees: 3
-- cars: 2
-- perfumes: 1
--- Part Two ---

-- As you're about to send the thank you note, something in the MFCSAM's instructions catches your eye. Apparently, it has an outdated retroencabulator, and so the output from the machine isn't exact values - some of them indicate ranges.

-- In particular, the cats and trees readings indicates that there are greater than that many (due to the unpredictable nuclear decay of cat dander and tree pollen), while the pomeranians and goldfish readings indicate that there are fewer than that many (due to the modial interaction of magnetoreluctance).

-- What is the number of the real Aunt Sue?


import Data.Char
import Data.List.Split
import Data.List

data Aunt = Aunt {
    sue         :: Int,
    children    :: Int,
    cats        :: Int,
    samoyeds    :: Int,
    pomeranians :: Int,
    akitas      :: Int,
    vizslas     :: Int,
    goldfish    :: Int,
    trees       :: Int,
    cars        :: Int,
    perfumes    :: Int}
    deriving (Show, Eq, Ord)

-- fSet :: String -> (Int -> Aunt -> Aunt)
fSet :: (String, Int) -> Aunt -> Aunt
fSet ("sue"        ,x) a = a { sue = x }
fSet ("children"   ,x) a = a { children = x }
fSet ("cats"       ,x) a = a { cats = x }
fSet ("samoyeds"   ,x) a = a { samoyeds = x }
fSet ("pomeranians",x) a = a { pomeranians = x }
fSet ("akitas"     ,x) a = a { akitas = x }
fSet ("vizslas"    ,x) a = a { vizslas = x }
fSet ("goldfish"   ,x) a = a { goldfish = x }
fSet ("trees"      ,x) a = a { trees = x }
fSet ("cars"       ,x) a = a { cars = x }
fSet ("perfumes"   ,x) a = a { perfumes = x }

isMyAunt1 :: Aunt -> Bool
isMyAunt1 a =
            (children    a == children    myAunt || children    a == 0)  
         && (cats        a == cats        myAunt || cats        a == 0) 
         && (samoyeds    a == samoyeds    myAunt || samoyeds    a == 0)  
         && (pomeranians a == pomeranians myAunt || pomeranians a == 0) 
         && (akitas      a == akitas      myAunt || akitas      a == 0) 
         && (vizslas     a == vizslas     myAunt || vizslas     a == 0) 
         && (goldfish    a == goldfish    myAunt || goldfish    a == 0) 
         && (trees       a == trees       myAunt || trees       a == 0) 
         && (cars        a == cars        myAunt || cars        a == 0) 
         && (perfumes    a == perfumes    myAunt || perfumes    a == 0)

isMyAunt2 :: Aunt -> Bool
isMyAunt2 a =
            (children    a == children    myAunt || children    a == 0)  
         && (cats        a >  cats        myAunt || cats        a == 0) 
         && (trees       a >  trees       myAunt || trees       a == 0) 
         && (goldfish    a <  goldfish    myAunt || goldfish    a == 0) 
         && (pomeranians a <  pomeranians myAunt || pomeranians a == 0) 
         && (samoyeds    a == samoyeds    myAunt || samoyeds    a == 0)  
         && (akitas      a == akitas      myAunt || akitas      a == 0) 
         && (vizslas     a == vizslas     myAunt || vizslas     a == 0) 
         && (cars        a == cars        myAunt || cars        a == 0) 
         && (perfumes    a == perfumes    myAunt || perfumes    a == 0) 

-- Using this to sort the most relevant is probably garbage
-- but it seems to work
zeroesAunt :: Aunt -> Int
zeroesAunt a = min 1 (children    a) +
               min 1 (cats        a) +
               min 1 (samoyeds    a) + 
               min 1 (pomeranians a) +
               min 1 (akitas      a) +
               min 1 (vizslas     a) +
               min 1 (goldfish    a) +
               min 1 (trees       a) +
               min 1 (cars        a) +
               min 1 (perfumes    a) 
               

aunt0 :: Aunt
aunt0 = Aunt { sue = 0, children = 0, cats = 0, samoyeds = 0, pomeranians = 0, akitas = 0, vizslas = 0, goldfish = 0, trees = 0, cars = 0, perfumes = 0 } 

myAunt :: Aunt
myAunt = Aunt { sue = 0, children = 3, cats = 7, samoyeds = 2, pomeranians = 3, akitas = 0, vizslas = 0, goldfish = 5, trees = 3, cars = 2, perfumes = 1 }

main :: IO ()
main = do
    input <- lines <$> readFile "./2015/day16/day.txt"
    let dataSet = map (map (\(x:y:_) -> (x, read y :: Int)) .
                       chunksOf 2 .
                       filter (not . null) . 
                       splitOneOf ":, " . 
                       map toLower) input
    let aunts = map (foldl (flip fSet) aunt0) dataSet   
    putStrLn "My aunt"
    print myAunt
    putStrLn "------------------"
    let aAunt1 = (sortOn zeroesAunt . filter isMyAunt1) aunts
    print "Answer Part 1:"
    print $ last aAunt1
    putStrLn "   "
    let aAunt2 = (sortOn zeroesAunt . filter isMyAunt2) aunts
    print "Answer Part 2:"
    print $ last aAunt2




