-- 9.12 Chapter Exercises --

import Data.Char

-- Data.Char

--1.
--isUpper :: Char -> Bool
--toUpper :: Char -> Char

-- 2.
f :: String -> String
f = filter isUpper

-- 3.
g :: String -> String
g (x:xs) = (toUpper x) : xs

--4.
h :: String -> String
h [] = []
h (x:xs) = (toUpper x) : h xs

--5.
i :: String -> Char
i xs = toUpper $ head xs

--5.
j :: String -> Char
j = toUpper . head 

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
