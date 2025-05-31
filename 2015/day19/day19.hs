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

-- Part 1
swap :: String -> String -> String -> [String]
swap x y xs = go x y xs "" []
    where
    go _ _ []         _    acc = acc
    go x y zss@(z:zs) left acc
        |x1 == x   = go x y x2 (left++x1) (acc ++ [left++y++x2])
        |otherwise = go x y zs (left++[z]) acc
        where l = length x
              x1 = take l zss
              x2 = drop l zss

doSwap :: Foldable t => String -> Int -> t (String, String) -> [String]
doSwap molecule freeze eqs  = nub $ concatMap (\x -> uncurry swap x molecule) eqs

-- Part 2
getToE :: [(String, String)] -> String -> [(String, Int)]
getToE eqs mol = go eqs mol [([], 0)]
    where go eqs mol acc = 
            case fst result of
                "e"   -> result : acc
                []    -> ("NONONONO!",999) : acc
                _     -> go eqs (fst result) (result : acc)
                where 
                    result = replaceAlln eqs (mol, 0)

replaceAlln :: [(String, String)] -> (String, Int) -> (String, Int)
replaceAlln [] (mol,n) = (mol, n)
replaceAlln ((from,to):rest) (mol,n) = replaceAlln rest result 
    where 
          result = swap2 from to mol ("",n)

swap2 :: (Eq a, Num b) => [a] -> [a] -> [a] -> ([a], b) -> ([a], b)
swap2 _    _   []         (acc, n) = (acc,n)
swap2 from to  zss@(z:zs) (left,n)  
    |x1 == from = swap2 from to x2 (left++to,n+1)
    |otherwise  = swap2 from to zs (left++[z],n)
    where l = length from
          x1 = take l zss
          x2 = drop l zss

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

    let eqs = fst dataSet
    let molecule = snd dataSet

    let part1 = length $ doSwap molecule 0 eqs
    print "Answer Part 1:"
    print part1

    print "Answer Part 2:"
    let eqs2 = sortOn (Down . length . fst) (map (\(a,b)->(b,a)) eqs)
    let part2 =  getToE eqs2 molecule
    print $ (sum . map snd) part2
    print "----"

    -- "fixed" grammar: make LHS and RHS tokens all single chars   
    let (eqsF, moleculeF) = fixGrammar dataSet
    let part1F = length $ doSwapF moleculeF 0 eqsF
    print "Answer Part 1, Fixed Grammar:"
    print part1F



------------------------------------------------------------------

-- Fixed grammar changing all to single characters.
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

hashE = hashPair "e"

doSwapF :: Foldable t => String -> Int -> t (String, String) -> [String]
doSwapF molecule freeze eqs  = nub $ concatMap (\x -> uncurry swapF x molecule) eqs

swapF :: String -> String -> String -> [String]
swapF x y xs = go x y xs "" []
    where
    go _ _ []         _    acc = acc
    go x y zss@(z:zs) left acc
        |[z] == x  = go x y zs (left++[z]) (acc ++ [left++y++zs])
        |otherwise = go x y zs (left++[z])  acc
