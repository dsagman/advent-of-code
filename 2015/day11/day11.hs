-- To help him remember his new password after the old one expires, Santa has devised a method of coming up with a password based on the previous one. Corporate policy dictates that passwords must be exactly eight lowercase letters (for security reasons), so he finds his new password by incrementing his old password string repeatedly until it is valid.

-- Incrementing is just like counting with numbers: xx, xy, xz, ya, yb, and so on. Increase the rightmost letter one step; if it was z, it wraps around to a, and repeat with the next letter to the left until one doesn't wrap around.

-- Unfortunately for Santa, a new Security-Elf recently started, and he has imposed some additional password requirements:

-- Passwords must include one increasing straight of at least three letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.
-- Passwords may not contain the letters i, o, or l, as these letters can be mistaken for other characters and are therefore confusing.
-- Passwords must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz.

-- For example:

-- hijklmmn meets the first requirement (because it contains the straight hij) but fails the second requirement requirement (because it contains i and l).
-- abbceffg meets the third requirement (because it repeats bb and ff) but fails the first requirement.
-- abbcegjk fails the third requirement, because it only has one double letter (bb).
-- The next password after abcdefgh is abcdffaa.
-- The next password after ghijklmn is ghjaabcc, because you eventually skip all the passwords that start with ghi..., since i is not allowed.

-- Given Santa's current password (your puzzle input), what should his next password be?
import Data.Char
import Data.List

incChar :: Char -> (Char, Bool) -- True -> carry
incChar x
    | x == 'z'               = ('a', True)
    | x `elem` ['h','n','k'] = ((chr . (2+) . ord) x, False) 
    | otherwise              = ((chr . (1+) . ord) x, False)

incStr :: String -> String
incStr xs = reverse $ aux (reverse xs) True 
    where 
        aux xs False  = xs
        aux (x:xs) True = nx : aux xs carry 
            where (nx, carry) = incChar x    

validP :: String -> Bool
validP xs = -- all (`notElem` xs) "ilo" && 
            -- we never allow i,l or o in incChar, so can skip check
            any (\(x,y,z) -> [y-x,z-y] == [1,1]) tripOrds && 
            ((>1) . length . filter (>1) . map length . group) xs
    where 
        tripOrds = zip3 os (drop 1 os) (drop 2 os)
        os = map ord xs

incP :: String -> [String]
incP = filter validP . iterate incStr

main :: IO ()
main = do
    let p = "hxbxwxba"
    print "Answer Part 1 and 2:" -- hxbxxyzz
    let p1_2 = take 2 $ incP p
    print p1_2


