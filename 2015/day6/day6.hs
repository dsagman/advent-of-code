-- Lights in your grid are numbered from 0 to 999 in each direction; the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. The instructions include whether to turn on, turn off, or toggle various inclusive ranges given as coordinate pairs. Each coordinate pair represents opposite corners of a rectangle, inclusive; a coordinate pair like 0,0 through 2,2 therefore refers to 9 lights in a 3x3 square. The lights all start turned off.

-- To defeat your neighbors this year, all you have to do is set up your lights by doing the instructions Santa sent you in order.

-- For example:

--     turn on 0,0 through 999,999 would turn on (or leave on) every light.
--     toggle 0,0 through 999,0 would toggle the first line of 1000 lights, turning off the ones that were on, and turning on the ones that were off.
--     turn off 499,499 through 500,500 would turn off (or leave off) the middle four lights.

-- After following the instructions, how many lights are lit?

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.List (foldl')

size = 10

toggle :: (Int,Int) -> (Int, Int) -> Map (Int, Int) Int -> Map (Int, Int) Int
toggle (x1,y1) (x2,y2) grid = 
    foldl' update grid keys
    where f _ x = Just (1 - x)
          keys = [(x,y) | x <- [x1..x2], y <- [y1..y2]]
          update g k = Map.updateWithKey f k g

turnOff :: (Int,Int) -> (Int, Int) -> Map (Int, Int) Int -> Map (Int, Int) Int
turnOff (x1,y1) (x2,y2) grid = 
    foldl' update grid keys
    where
        keys = [(x,y) | x <- [x1..x2], y <- [y1..y2]]
        update g k = Map.adjust (const 0) k g        

turnOn :: (Int,Int) -> (Int, Int) -> Map (Int, Int) Int -> Map (Int, Int) Int
turnOn (x1,y1) (x2,y2) grid = 
    foldl' update grid keys
    where
        keys = [(x,y) | x <- [x1..x2], y <- [y1..y2]]
        update g k = Map.adjust (const 1) k g    

lights :: [((Int, Int), Int)]
lights = [((x,y),0) | x <- [0..(size-1)], y <-[0..(size-1)]]

lightsMap :: Map (Int, Int) Int
lightsMap = Map.fromList lights 

main :: IO ()
main = do
    displayMap $ turnOff (3,3) (6,6) $ toggle (0,0) (9,9) lightsMap


displayMap :: Map (Int, Int) Int -> IO ()
displayMap m = mapM_ putStrLn [ row y | y <- [0..(size - 1)] ]
  where
    row y = [ display (Map.findWithDefault 0 (x, y) m) | x <- [0..(size - 1)] ]
    display 0 = '.'  -- off
    display _ = '#'  -- on


