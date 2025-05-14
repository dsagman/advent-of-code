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

-- import Control.Applicative
import Data.Aeson
-- import Data.Text 
import qualified Data.ByteString.Lazy as B

decoder :: Either a b -> b
decoder (Left _) = error "Error"
decoder (Right x) = x


main :: IO ()
main = do
    input <- B.readFile "./2015/day12/test.json"    
    -- input <- B.readFile "./2015/day12/day.json"
    let r = eitherDecode input :: Either String [Value]
    print $ fmap show r
    print "Part 1 Answer:"



    print "Part 2 Answer:"
   