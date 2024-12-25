import Data.List (sort, nub, (\\))
import Data.List.Extra (intersect)
parseEdges :: String -> [(String, String)]
parseEdges = map parseEdge . lines
    where
        parseEdge :: String -> (String, String)
        parseEdge line = (take 2 line, drop 3 line)

main :: IO ()
main = do
    -- let dataFile = "2024/day23/test"
    let dataFile = "2024/day23/day.txt"
    input <- readFile dataFile
    let aGraph = parseEdges input
    let aNodes = nodes aGraph
    let aNodesT = filter (\x -> head x == 't') aNodes
    -- print $  aNodes
    let aNmap = map (\x -> (x, neighbors aGraph x)) aNodes
    -- print aNmap
    -- print $ "Part 1: " ++ show (length aThreesT)
    let aThreesT' = threeCycle' aNmap aNodesT
    print $ "Part 1: " ++ show (length aThreesT')
    let aBronKerbosch = bronKerboschi [] aNodes [] aNmap
    print $ (nub . map sort) $ filter (\x -> length x == maximum (map length aBronKerbosch)) aBronKerbosch
    -- print input

-- *Without Pivoting
-- algorithm BronKerbosch1(R, P, X) is
--     if P and X are both empty then
--         report R as a maximal clique
--     for each vertex v in P do
--         BronKerbosch1(R ⋃ {v}, P ⋂ N(v), X ⋂ N(v))
--         P := P \ {v}
--         X := X ⋃ {v}
bronKerboschi :: [String] -> [String] -> [String] -> [(String, [String])] -> [[String]]
bronKerboschi r p x nMap
    | null p && null x = [r]
    | otherwise = concatMap (\v -> 
        let newP = p `intersect` neighbors' nMap v
            newX = x `intersect` neighbors' nMap v
            remainingP = p \\ [v]
            updatedX = v : x
        in bronKerboschi (v : r) newP newX nMap
        ) p

neighbors' :: [(String, [String])] -> String -> [String]
neighbors' nMap v = snd $ head $ filter (\(k,_) -> k == v) nMap

nodes :: [(String, String)] -> [String]
nodes aGraph = (nub . sort) $ concatMap (\(a, b) -> [a, b]) aGraph

threeCycle' :: [(String, [String])] -> [String] -> [[String]]
threeCycle' nMap nodes = (nub . sort) [sort [a, b, c] |
                    a <- nodes,
                    b <- neighbors' nMap a,
                    c <- neighbors' nMap b,
                    d <- neighbors' nMap c,
                    a == d]

neighbors :: [(String, String)] -> String -> [String]
neighbors [] _ = []
neighbors ((a, b):graph') node
    | a == node = b : neighbors graph' node
    | b == node = a : neighbors graph' node
    | otherwise = neighbors graph' node




findCycles graph node path
    | length path > 1 && node == last path = [path] -- Found a cycle
    | node `elem` path = [] -- Avoid revisiting nodes in the current path
    -- | length path == 10 = [] -- Prevent infinite recursion
    | otherwise =
        concat [findCycles (removeEdge graph (node, n)) n (node:path) |
                n <- neighbors graph node]
  where
    removeEdge g (x, y) = filter (\(a, b) -> not ((a == x && b == y) || (a == y && b == x))) g

