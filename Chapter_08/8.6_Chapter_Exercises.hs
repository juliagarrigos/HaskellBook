import Data.List (intersperse)

-- Reviewing currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

--3
-- frappe (appedCatty "2")
-- frappe (cattyConny "woops" "2")
-- frape ( "woops mrow 2")
-- flippy "haha" "woops mrow 2"
-- flip cattyConny "haha" "woops mrow 2"
-- cattyConny "woops mrow 2" "haha" 
-- "woops mrow 2 mrow haha" 

-- -- 5
-- cattyConny (frappe "pink")(cattyConny "green" (appedCatty "blue"))
-- cattyConny (frappe "pink")(cattyConny "green" (cattyConny "woops" "blue"))
-- cattyConny (frappe "pink")(cattyConny "green" "woops mrow blue")
-- cattyConny (frappe "pink") "green mrow woops mrow blue"
-- cattyConny (flippy "haha" "pink") "green mrow woops mrow blue"
-- cattyConny (flip cattyConny "haha" "pink") "green mrow woops mrow blue"
-- cattyConny (cattyConny "pink" "haha") "green mrow woops mrow blue"
-- cattyConny "pink mrow haha" "green mrow woops mrow blue"
-- "pink mrow haha mrow green mrow woops mrow blue"

-- --6
-- cattyConny (flippy "Pugs" "are") "awesome"
-- cattyConny (flip cattyConny "Pugs" "are") "awesome"
-- cattyConny (cattyConny "are" "Pugs") "awesome"
-- "are mrow Pugs mrow awesome"

-- Recursion

--1.

-- dividedBy 15 2
-- go 15 2 0
-- go (15-2) 2 (0+1)
-- go 13 2 1
-- go (13-2) 2 (1+1)
-- go 11 2 2
-- go (11-2) 2 (2+1)
-- go 9 2 3
-- go (9-2) 2 (3+1)
-- go 7 2 4
-- go (7-2) 2 (4+1)
-- go 5 2 5
-- go (5-2) 2 (5+1)
-- go 3 2 6 
-- go (3-2) 2 6+1
-- go 1 2 7
-- (7,1)

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