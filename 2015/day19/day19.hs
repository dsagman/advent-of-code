{-
For example, imagine a simpler machine that supports only the following replacements:

H => HO
H => OH
O => HH

Given the replacements above and starting with HOH, the following molecules could be generated:

    HOOH (via H => HO on the first H).
    HOHO (via H => HO on the second H).
    OHOH (via H => OH on the first H).
    HOOH (via H => OH on the second H).
    HHHH (via O => HH).

So, in the example above, there are 4 distinct molecules (not five, because HOOH appears twice) after one replacement from HOH. Santa's favorite molecule, HOHOHO, can become 7 distinct molecules (over nine replacements: six from H, and three from O).

The machine replaces without regard for the surrounding characters. For example, given the string H2O, the transition H => OO would result in OO2O.

Your puzzle input describes all of the possible replacements and, at the bottom, the medicine molecule for which you need to calibrate the machine. How many distinct molecules can be created after all the different ways you can do one replacement on the medicine molecule?

- Part Two ---

Now that the machine is calibrated, you're ready to begin molecule fabrication.

Molecule fabrication always begins with just a single electron, e, and applying replacements one at a time, just like the ones during calibration.

For example, suppose you have the following replacements:

e => H
e => O
H => HO
H => OH
O => HH

If you'd like to make HOH, you start with e, and then make the following replacements:

    e => O to get O
    O => HH to get HH
    H => OH (on the second H) to get HOH

So, you could make HOH after 3 steps. Santa's favorite molecule, HOHOHO, can be made in 6 steps.

How long will it take to make the medicine? Given the available replacements and the medicine molecule in your puzzle input, what is the fewest number of steps to go from e to the medicine molecule?
-}

import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)
-- import Data.List
import Data.List.Extra
import Data.Ord
import Data.Char
import Control.Monad

swapParser :: Parser (String, String)
swapParser = do
    x <- many1 letter
    string " => "
    y <- many1 letter
    pure (x, y)

dataParser :: Parser String
dataParser = many1 letter

inputParser :: Parser ([(String, String)], String)
inputParser = do
    swaps <- many (try (swapParser <* newline))
    newline
    d <- dataParser
    return (swaps, d)

-- Hard coding the nonterminals on RHS from dataset.
-- If this works, then I should extract them algorithmically
-- But for now, just trying to get the nonterminals to single chars
fixRHSPairs :: [(String, String)]
fixRHSPairs = [("Ar", hashPair "Ar"),
            ("CRn", hashPair "CRn"),
            ("Rn", hashPair "Rn")]

fixGrammar :: ([(String, String)], String) -> ([(String, String)], String)
fixGrammar (eqs,molecule) = (fixEqs, fixMolecule)
    where
    fixLeftEqs  = [hashPair from | (from,_) <- eqs]
    fixEqsMap   = zip (map fst eqs) fixLeftEqs
    fixRightEqs = [foldl' (\acc (from,to) -> replace from to acc) rhs (fixEqsMap++fixRHSPairs) | rhs <- map snd eqs]
    fixEqs      = zip fixLeftEqs fixRightEqs
    fixMolecule = foldl' (\acc (from, to) -> replace from to acc) molecule (fixEqsMap ++ fixRHSPairs)


hashPair :: String -> String
hashPair xs
  | length xs == 1 = [chr (256 * ord (head xs)) ]
  | length xs == 2 = [chr (256 * ord (head xs) + ord (xs !! 1)) ]
  | length xs == 3 = [chr (256 * ord (head xs) + ord (xs !! 1) + ord (xs !! 2)) ]
  | otherwise = "NOPE!"

-- For "unfixed" grammar
-- swap :: String -> String -> String -> [String]
-- swap x y xs = go x y xs "" []
--     where
--     go _ _ []         _    acc = acc
--     go x y zss@(z:zs) left acc
--         |x1 == x   = go x y x2 (left++x1) (acc ++ [left++y++x2])
--         |otherwise = go x y zs (left++[z]) acc
--         where l = length x
--               x1 = take l zss
--               x2 = drop l zss

-- "fixed grammar"
swap :: String -> String -> String -> [String]
swap x y xs = go x y xs "" []
    where
    go _ _ []         _    acc = acc
    go x y zss@(z:zs) left acc
        |[z] == x  = go x y zs (left++[z]) (acc ++ [left++y++zs])
        |otherwise = go x y zs (left++[z])  acc

-- "unfixed grammar"
-- swapFreeze :: String -> String -> String -> Int -> [String]
-- swapFreeze x y xs n = map (\yss -> take n xs ++ yss) $ go x y (drop n xs) "" []
--     where
--     go _ _ []         _    acc = acc
--     go x y zss@(z:zs) left acc
--         |x1 == x   = go x y x2 (left++x1) (acc ++ [left++y++x2])
--         |otherwise = go x y zs (left++[z]) acc
--         where l = length x
--               x1 = take l zss
--               x2 = drop l zss

-- "fixed grammar"
swapFreeze :: String -> String -> String -> Int -> [String]
swapFreeze x y xs n = map (\yss -> take n xs ++ yss) $  go x y (drop n xs) "" []
    where
    go _ _ []         _    acc = acc
    go x y zss@(z:zs) left acc
        |[z] == x  = go x y zs (left++[z]) (acc ++ [left++y++zs])
        |otherwise = go x y zs (left++[z])  acc

-- doSwap :: Foldable t => String -> Int -> t (String, String) -> [String]
doSwap molecule freeze eqs  = nub $ concatMap (\x -> uncurry swap x molecule) eqs
-- 
doSwapFreeze molecule freeze eqs  = nub $ concatMap (\x -> uncurry swapFreeze x molecule freeze) eqs

main :: IO ()
main = do
    -- input <- readFile "./2015/day19/test"
    -- input <- readFile "./2015/day19/test2"
    -- input <- readFile "./2015/day19/test3"
    input <- readFile "./2015/day19/day.txt"

    dataSet <- case parse inputParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x

    -- "unfixed" grammar
    let eqs = fst dataSet
    let molecule = snd dataSet
    let electrons = (map snd . filter ((=="e") . fst)) eqs


    -- "fixed" grammar: make LHS and RHS tokens all single chars   
    -- let (eqs, molecule) = fixGrammar dataSet
    -- let electrons = (map snd . filter ((==hashPair "e") . fst)) eqs

    -- mapM_ print eqs
    -- print molecule

    let part1 = length $ doSwap molecule 0 eqs
    print "Answer Part 1:"
    -- print part1

    print "Answer Part 2:"
    let rEqs = sortOn (length . snd) $  map (\(lhs,rhs) -> (lhs, reverse rhs)) eqs
    let rElectrons = map reverse electrons
    print electrons
    print rElectrons
    let rMol = reverse molecule
    let gM = greedyMatch rEqs rMol rElectrons 0
    print $ length rMol
    print $ gM
    print $ [(lhs ++ drop (length rhs) rMol)
                  | (lhs, rhs) <- rEqs
                  , rhs == take (length rhs) rMol
                  ]

    print "----"

hashE = hashPair "e"

greedyMatch _ rMol ans n | rMol `elem` ans = n
greedyMatch rEqs rMol ans n =
    let results = [ greedyMatch rEqs (lhs ++ drop (length rhs) rMol) ans (n+1)
                  | (lhs, rhs) <- rEqs
                  , rhs == take (length rhs) rMol
                  ]
    in if null results then 99999 else minimum results

-- greedyMatch [] _ ans n = 99999
-- greedyMatch rEqs rMol ans n
--             | rMol == hashE  = n
--             | otherwise      = 
--               case findMatch rEqs rMol of
--                 Nothing -> greedyMatch (last rEqs:init rEqs) rMol ans n
--                 Just (lhs,rhs) -> greedyMatch rEqs (lhs ++ drop (length rhs) rMol) ans (n+1)
               

findMatch [] _ = Nothing
findMatch ((lhs,rhs):rest) rMol
    | rhs == take (length rhs) rMol = Just (lhs,rhs)
    | otherwise                     = findMatch rest rMol





-- makeMol :: [String] -> [(String, String)] -> String -> Int -> (Int, [String])
makeMol electrons eqs molecule i matched
    | i == 40                   = (i+1, ["Failed!"], matched)
    | molecule `elem` electrons = (i+1, ["Done!"], matched)
    | otherwise                 =  makeMol next' eqs molecule (i+1) newMatched
    --                                   v Freeze amount
    where next = nub $ concat [doSwapFreeze e (matched-2) eqs | e <- electrons]
          (next', newMatched) = molMatch next molecule


molMatch electrons molecule =
    --  filter (\x -> length x < length molecule) $ 
     ( nub $
     map (take (mostMatches+6) . snd)  -- amount of each match to use
     (takeWhile (\x -> fst x >= (mostMatches-2)) nMatchTuple), -- allow matches of match length
    --  (electrons,
     mostMatches)
     where
        mostMatches = if null nMatchTuple then 0 else (fst . head) nMatchTuple
        nMatchTuple = sortOn (Down . fst) [(nMatch e molecule,e) | e <- electrons]
        nMatch e = length . takeWhile id . zipWith (==) e

checkMol electrons m = sortOn (Down . fst) [(nMatch e m, e) | e <- electrons]
    where
        nMatch e = length . takeWhile id . zipWith (==) e

myM = "CRnCaCaCaSiRnBPTiMgArSiRnSiRnMgArSiRnCaFArTiTiBSiThFYCaFArCaCaSiThCaPBSiThSiThCaCaPTiRnPBSiThRnFArArCaCaSiThCaSiThSiRnMgArCaPTiBPRnFArSiThCaSiRnFArBCaSiRnCaPRnFArPMgYCaFArCaPTiTiTiBPBSiThCaPTiBPBSiRnFArBPBSiRnCaFArBPRnSiRnFArRnSiRnBFArCaFArCaCaCaSiThSiThCaCaPBPTiTiRnFArCaPTiBSiAlArPBCaCaCaCaCaSiRnMgArCaSiThFArThCaSiThCaSiRnCaFYCaSiRnFYFArFArCaSiRnFYFArCaSiRnBPMgArSiThPRnFArCaSiRnFArTiRnSiRnFYFArCaSiRnBFArCaSiRnTiMgArSiThCaSiThCaFArPRnFArSiRnFArTiTiTiTiBCaCaSiRnCaCaFYFArSiThCaPTiBPTiBCaSiThSiRnMgArCaF"

myE = ["HCaCaF","HPBF","HPRnFArF","HSiRnFYFArF","HSiRnMgArF","HSiThF","HCaCaF","HCaPMg","HCaSiAl","CRnAlArCaF","CRnFYFYFArCaF","CRnFYMgArCaF","CRnMgYFArCaF","HCaCaF","NRnFYFArCaF","NRnMgArCaF","NThCaF","OBCaF","ORnFArCaF","CRnAlArPMg","CRnFYFYFArPMg","CRnFYMgArPMg","CRnMgYFArPMg","HCaPMg","NRnFYFArPMg","NRnMgArPMg","NThPMg","OBPMg","ORnFArPMg","HPBF","HPTiMg","HCaPMg","HPTiMg","HSiRnFArMg","HSiThF","HSiThRnFAr","CRnAlArSiAl","CRnFYFYFArSiAl","CRnFYMgArSiAl","CRnMgYFArSiAl","HCaSiAl","NRnFYFArSiAl","NRnMgArSiAl","NThSiAl","OBSiAl","ORnFArSiAl","HCaSiAl","CRnThFArF","CRnThRnFArArF","CRnAlArCaF","CRnAlArPMg","CRnAlArSiAl","CRnCaFYFYFArF","CRnFYCaFYFArF","CRnFYFYCaFArF","CRnFYFYFArCaF","CRnPMgYFYFArF","CRnFYPMgYFArF","CRnFYFYPMgArF","CRnFYFYFArPMg","CRnSiAlYFYFArF","CRnFYSiAlYFArF","CRnFYFYSiAlArF","CRnFYFYFArSiAl","CRnCaFYMgArF","CRnFYMgArCaF","CRnPMgYMgArF","CRnFYMgArPMg","CRnSiAlYMgArF","CRnFYMgArSiAl","CRnFYBFArF","CRnFYTiMgArF","CRnMgYCaFArF","CRnMgYFArCaF","CRnMgYPMgArF","CRnMgYFArPMg","CRnMgYSiAlArF","CRnMgYFArSiAl","CRnBFYFArF","CRnTiMgYFArF","HCaCaF","HPBF","HPRnFArF","HSiRnFYFArF","HSiRnMgArF","HSiThF","HCaCaF","HCaPMg","HCaSiAl","CRnAlArCaF","CRnFYFYFArCaF","CRnFYMgArCaF","CRnMgYFArCaF","HCaCaF","NRnFYFArCaF","NRnMgArCaF","NThCaF","OBCaF","ORnFArCaF","NRnCaFYFArF","NRnFYCaFArF","NRnFYFArCaF","NRnPMgYFArF","NRnFYPMgArF","NRnFYFArPMg","NRnSiAlYFArF","NRnFYSiAlArF","NRnFYFArSiAl","CRnFArRnFYFArF","HSiRnFYFArF","NRnMgArCaF","NRnMgArPMg","NRnMgArSiAl","NRnBFArF","NRnTiMgArF","CRnFArRnMgArF","HSiRnMgArF","NThCaF","NThPMg","NThSiAl","CRnFArThF","HSiThF","NThCaF","OBCaF","OTiBF","OTiRnFArF","OBCaF","OBPMg","OBSiAl","CRnFYFArBF","CRnMgArBF","HPBF","NRnFArBF","OTiBF","ORnCaFArF","ORnFArCaF","ORnPMgArF","ORnFArPMg","ORnSiAlArF","ORnFArSiAl","CRnFYFArRnFArF","CRnMgArRnFArF","HPRnFArF","NRnFArRnFArF","OTiRnFArF","NThCaF","NThPMg","NThSiAl","CRnFArThF","HSiThF","NThCaF","NThRnCaFAr","NThRnPMgAr","NThRnSiAlAr","CRnFArThRnFAr","HSiThRnFAr","NThCaRnFAr","CRnFArThF","CRnFArThRnFAr","CRnCaFArAl","CRnPMgArAl","CRnSiAlArAl","HSiThF","HSiThRnFAr","CRnAlArSiAl","CRnFYFYFArSiAl","CRnFYMgArSiAl","CRnMgYFArSiAl","HCaSiAl","NRnFYFArSiAl","NRnMgArSiAl","NThSiAl","OBSiAl","ORnFArSiAl","HCaSiAl","OBCaF","OTiBF","OTiRnFArF","OBCaF","OBPMg","OBSiAl","CRnFYFArBF","CRnMgArBF","HPBF","NRnFArBF","OTiBF","OTiBF","OTiTiMg","CRnFYFArTiMg","CRnMgArTiMg","HPTiMg","NRnFArTiMg","OTiTiMg","OBPMg","OTiTiMg","CRnCaFYFArMg","CRnFYCaFArMg","CRnPMgYFArMg","CRnFYPMgArMg","CRnSiAlYFArMg","CRnFYSiAlArMg","CRnFYFArBF","CRnFYFArTiMg","CRnBFArMg","CRnMgArBF","CRnTiMgArMg","CRnMgArTiMg","CRnAlArPMg","CRnFYFYFArPMg","CRnFYMgArPMg","CRnMgYFArPMg","HCaPMg","NRnFYFArPMg","NRnMgArPMg","NThPMg","OBPMg","ORnFArPMg","HPBF","HPTiMg","HCaPMg","HPTiMg","HSiRnFArMg","NRnCaFArMg","NRnPMgArMg","NRnSiAlArMg","NRnFArBF","NRnFArTiMg","CRnFArRnFArMg","HSiRnFArMg","OTiBF","OTiTiMg","CRnFYFArTiMg","CRnMgArTiMg","HPTiMg","NRnFArTiMg","OTiTiMg","OBPMg","OTiTiMg"]

