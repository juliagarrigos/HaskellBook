import Data.List (intersperse)
-- (TODO posar el que vaig fer a casa) 8.6 Chapter Exercises --

--TODOOO


-- Recursion

--1.

--2.
-- mySum 5 = 1 + 2 + 3 + 4 + 5 = 15
mySum :: (Eq a, Num a) => a -> a
mySum 1 = 1
mySum x = mySum(x-1) + x

--3.
-- myMult 2 3 = 2 + 2 + 2 = 6
myMult :: Integral a => a -> a -> a
myMult x y = go 0 0 
    where go n c 
            | c == y = n
            | otherwise = go (n+x) (c+1)

-- myMult 2 3 = 3 + 3 = 6
myMult' :: Integral a => a -> a -> a
myMult' x y = go x y 0 
    where go x' y' n
            | x' == 0 = n
            | otherwise = go (x'- 1) y'(n + y')


  -- Fixing dividedBy
data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy :: Integer -> Integer -> DividedResult 
dividedBy num denom = go (abs num) (abs denom) 0
    where go n d count
            | d == 0 = DividedByZero
            | n < d = if (denom < 0 && num > 0) || (denom > 0 && num < 0) 
                      then  Result (negate count)
                      else Result count
            | otherwise = go (n - d) d (count + 1)

testing x y = print ("Div result: " ++ show (div x y) ++ "    DividedBy result: " ++ show (dividedBy x y))

-- McCarthy 91 function

mc n 
    | n > 100 = n - 10
    | otherwise = mc (mc (n+11))
 



-- Numbers into words
digitToWord :: Int -> String
digitToWord n = case n of
                0 -> "Zero"
                1 -> "One"
                2 -> "Two"
                3 -> "Three"
                4 -> "Four"
                5 -> "Five"
                6 -> "Six"
                7 -> "Seven"
                8 -> "Eight"
                9 -> "Nine"
                otherwise -> ""

digits :: Int -> [Int]
digits n = go n [] 
    where
        go :: Int -> [Int] -> [Int] 
        go 0 l = l
        go n' l = go (div n' 10)  l ++ [mod n' 10] 

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits