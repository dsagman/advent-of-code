
-- https://www.reddit.com/r/adventofcode/comments/5j4lp1/comment/dbdf4up/
steal l = let l' = filter (/= (l !! div (length l) 2)) l  -- kill an elf
          in drop 1 l' ++ take 1 l'                       -- rotate killer to back
          
solve n = head $ until ((==1) . length) steal [1..n]

main = print $  map solve [1..100]