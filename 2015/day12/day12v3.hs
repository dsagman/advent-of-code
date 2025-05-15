{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import System.Exit (exitFailure)
import Data.Bifunctor ( Bifunctor(first) )

-- Custom JSON type
data J
  = JObject (HM.HashMap String J)
  | JArray [J]
  | JNumber Integer
  | JString String
  deriving (Show, Generic, Eq)

-- FromJSON instance to parse into custom type
instance FromJSON J where
  parseJSON (Number n) = pure $ JNumber (round n)
  parseJSON (String s) = pure $ JString (show s)
  parseJSON (Array a)  = JArray <$> traverse parseJSON (V.toList a)
  parseJSON (Object o) = JObject <$> traverse parseJSON (HM.fromList $ map (first show) (KM.toList o))
  parseJSON _ = fail "Unsupported JSON value"

-- Sum all numbers
sumJ :: J -> Integer
sumJ (JNumber n) = n
sumJ (JObject o) = sum (map sumJ (HM.elems o))
sumJ (JArray xs) = sum (map sumJ xs)
sumJ _           = 0

-- Remove any object with a value "red"
noRedGroups :: J -> J
noRedGroups (JNumber n) = JNumber n
noRedGroups (JString s) = JString s
noRedGroups (JArray xs) = JArray (map noRedGroups xs)
noRedGroups (JObject o)
  | JString "\"red\"" `elem` HM.elems o = JNumber 0
  | otherwise = JObject (fmap noRedGroups o)

-- Main program
main :: IO ()
main = do
  input <- B.readFile "./2015/day12/day.json"
  jData <- case eitherDecode input of
    Left err -> putStrLn ("Parse error: " ++ err) >> exitFailure
    Right val -> pure val :: IO J

  print jData
  putStrLn "Part 1 Answer:"
  print $ sumJ jData

  putStrLn "Part 2 Answer:"
  print $ sumJ (noRedGroups jData)
