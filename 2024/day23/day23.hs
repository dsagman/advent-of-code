import Data.List (sort, nub)
import Data.Map.Internal.Debug (node)
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
    let aThreesT = threeCycle aGraph aNodesT
    -- print aThreesT
    print $ "Part 1: " ++ show (length aThreesT)
    -- print input

nodes :: [(String, String)] -> [String]
nodes aGraph = (nub . sort) $ concatMap (\(a, b) -> [a, b]) aGraph

neighbors :: [(String, String)] -> String -> [String]
neighbors [] _ = []
neighbors ((a, b):xs) node
    | a == node = b : neighbors xs node
    | b == node = a : neighbors xs node
    | otherwise = neighbors xs node

threeCycle :: [(String, String)] -> [String] -> [[String]]
threeCycle graph nodes = (nub . sort) [sort [a, b, c] | 
                    a <- nodes,
                    b <- neighbors graph a,
                    c <- neighbors graph b,
                    d <- neighbors graph c,
                    a == d]


        
