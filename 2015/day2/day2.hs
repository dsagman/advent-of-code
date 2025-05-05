-- The elves are running low on wrapping paper, and so they need to submit an order for more. 
-- They have a list of the dimensions (length l, width w, and height h) of each present, 
-- and only want to order exactly as much as they need.

-- Fortunately, every present is a box (a perfect right rectangular prism), 
-- which makes calculating the required wrapping paper for each gift a little easier: 
-- find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. 
-- The elves also need a little extra paper for each present: the area of the smallest side.

-- For example:

-- A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of wrapping paper 
-- plus 6 square feet of slack, for a total of 58 square feet.
-- A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of wrapping paper 
-- plus 1 square foot of slack, for a total of 43 square feet.

-- All numbers in the elves' list are in feet. How many total square feet of wrapping paper should they order?

-- The ribbon required to wrap a present is the 
-- shortest distance around its sides, 
-- or the smallest perimeter of any one face. 
-- Each present also requires a bow made out of ribbon as well; 
-- the feet of ribbon required for the perfect bow 
-- is equal to the cubic feet of volume of the present. 
-- Don't ask how they tie the bow, though; they'll never tell.

-- For example:

--     A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon 
--     to wrap the present plus 2*3*4 = 24 feet of ribbon for the bow, 
--     for a total of 34 feet.
--     A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon 
--     to wrap the present plus 1*1*10 = 10 feet of ribbon for the bow, 
--     for a total of 14 feet.

-- How many total feet of ribbon should they order?
import Text.Parsec
import Text.Parsec.String (Parser)
import System.Exit (exitFailure)
import Data.List (sort)


dimParser :: Parser [Int]
dimParser = do
    l <- read <$> many1 digit
    char 'x'
    w <- read <$> many1 digit
    char 'x'
    h <- read <$> many1 digit
    return [l,w,h]

inputParser :: Parser [[Int]]
inputParser =  do
    many $ try (dimParser <* newline)

paperSize :: [Int] -> Int
paperSize (a:b:c:_) = 3 * a * b + 2 * b * c + 2 * c * a

ribbonLength :: [Int] -> Int
ribbonLength (a:b:c:_) = 2 * a + 2 * b + a * b * c

main :: IO ()
main = do
    input <- readFile "./2015/day2/day.txt"
    dataSet <- case parse inputParser "" input of
        Left err -> do
            putStrLn $ "Error parsing input: " ++ show err
            exitFailure
        Right x -> pure x
    print "Answer 1:"
    print $ (sum . map (paperSize . sort)) dataSet
    print "Answer 2:"
    print $ (sum . map (ribbonLength . sort)) dataSet