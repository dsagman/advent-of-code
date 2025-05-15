{-# LANGUAGE OverloadedStrings #-}
-- Using JSON Parser
-- They have a JSON document which contains a variety of things: arrays ([1,2,3]), objects ({"a":1, "b":2}), numbers, and strings. Your first job is to simply find all of the numbers throughout the document and add them together.

-- For example:

--     [1,2,3] and {"a":2,"b":4} both have a sum of 6.
--     [[[3]]] and {"a":{"b":4},"c":-1} both have a sum of 3.
--     {"a":[-1,1]} and [-1,{"a":1}] both have a sum of 0.
--     [] and {} both have a sum of 0.

-- You will not encounter any strings containing numbers.

-- What is the sum of all numbers in the document?

import Data.Aeson ( eitherDecode, Value(Array, Object, String, Number) )
import Data.Aeson.KeyMap (elems)
import qualified Data.ByteString.Lazy as B
import System.Exit (exitFailure)

sumJ :: Value -> Integer
sumJ (Number n)  = toInteger $ round n
sumJ (String _)  = 0
sumJ (Array js)  = sum (sumJ <$> js)
sumJ (Object js) = sum (sumJ <$> js)

noRedGroups :: Value -> Value
noRedGroups (Number n) = Number n
noRedGroups (String _) = Number 0
noRedGroups (Array js) = Array (noRedGroups <$> js)
noRedGroups (Object js)
  | String "red" `elem` elems js = Number 0
  | otherwise = Object (noRedGroups <$> js)

main :: IO ()
main = do
    input <- B.readFile "./2015/day12/day.json"
    jData <- case eitherDecode input :: Either String Value of
        Left err -> putStrLn ("Error parsing input: " ++ show err) >> exitFailure
        Right x  -> pure x
    -- print $ jData
    print "Part 1 Answer:"
    print $ sumJ jData 

    print "Part 2 Answer:"
    print $ sumJ (noRedGroups jData)
   