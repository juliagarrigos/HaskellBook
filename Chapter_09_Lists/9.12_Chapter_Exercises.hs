-- 9.12 Chapter Exercises --

import Data.Char

-- Data.Char

--1.
--isUpper :: Char -> Bool
--toUpper :: Char -> Char

-- 2.
capitalLetters :: String -> String
capitalLetters = filter isUpper

-- 3.
capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = (toUpper x) : xs

--4.
capitalizeAll :: String -> String
capitalizeAll "" = ""
capitalizeAll (x:xs) = (toUpper x) : capitalizeAll xs

--5.
capitalHead :: String -> Char
capitalHead xs = toUpper $ head xs

--5.
capitalHead' :: String -> Char
capitalHead' = toUpper . head 

-- Ciphers
caesarRight :: String -> Int -> String
caesarRight [] _ = []
caesarRight (x:xs) s = shiftChar x s : caesarRight xs s

shiftChar :: Char -> Int -> Char
shiftChar c s = chr $ mod (ord c - ord 'a' + s ) 26 +  ord 'a'

unshiftChar :: Char -> Int -> Char
unshiftChar c s = chr $ mod (ord c - ord 'a' - s ) 26 +  ord 'a'

uncaesarRight :: String -> Int -> String
uncaesarRight [] _ = []
uncaesarRight (x:xs) s = unshiftChar x s : uncaesarRight xs s

testing = uncaesarRight (caesarRight "helloworld" 7) 7 == "helloworld"

-- Writing your own standard functions

-- 1.
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

--2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)=  f x || myAny f xs

--3. 
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem e (x:xs) = e == x || myElem e xs 

myElem' :: Eq a => a -> [a] -> Bool
myElem' _ [] = False
myElem' e xs = any (==e) xs 

--4.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

--5.
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs 

--6.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

--7.
squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain xs = squishMap id xs

--8.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = undefined
myMaximumBy _ (x:[]) = x
myMaximumBy f (x:xs) = if f x (head xs) == GT
                       then  myMaximumBy f (x : tail xs)
                       else myMaximumBy f (head xs : tail xs)

--9.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = undefined
myMinimumBy _ (x:[]) = x
myMinimumBy f (x:xs) = if f x (head xs) == LT
    then  myMinimumBy f (x : tail xs)
    else myMinimumBy f (head xs : tail xs)

--10.
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare
myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
