-- 10.10 Chapter Exercises --

-- Warm-up and review
stops = "pbtdkg"
vowels = "aeiou"

--a)
stopsAndVowels :: [(Char, Char, Char)]
stopsAndVowels = [(x, y, z) | x <- stops, y <- vowels, z <- stops]

--b)
stopsAndVowels' :: [(Char, Char, Char)]
stopsAndVowels' = [(x, y, z) | x <- stops, y <- vowels, z <- stops, x == 'p']

--c)
nouns :: [String]
nouns = ["Alice","Bob","Charles"]

verbs :: [String]
verbs = ["wins", "kisses", "loves"]

nounsAndVerbs :: [(String, String, String)]
nounsAndVerbs = [(x, y, z) | x <- nouns, y <- verbs, z <- nouns]

--3
seekritFunc :: Fractional a => String -> a
seekritFunc x = fromIntegral(sum (map length (words x))) / fromIntegral(length (words x))

-- Rewriting functions using folds
--1
myOr :: [Bool] -> Bool
myOr xs = foldr (||) False xs

myOrPl :: [Bool] -> Bool
myOrPl = foldr (||) False

myOr' :: [Bool] -> Bool
myOr' [] = False
myOr' (x:xs) = x || myOr' xs

--2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> (f x) || b) False xs

myAnyPl :: (a -> Bool) -> [a] -> Bool
myAnyPl f = foldr (\x b -> (f x) || b) False

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' _  [] = False
myAny' f (x:xs) = f x || myAny' f xs

--3.
myElem :: Eq a => a -> [a] -> Bool
myElem e xs = foldr (\x b -> (x == e) || b) False xs

myElemPl :: Eq a => a -> [a] -> Bool
myElemPl e = foldr (\x b -> (x == e) || b) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' e xs = any (==e) xs

--4.
myReverse :: [a] -> [a]
myReverse xs = foldr (\x b -> b ++ [x]) [] xs

myReverse' :: [a] -> [a]
myReverse' xs = foldl (flip (:)) [] xs

myReversePl :: [a] -> [a]
myReversePl = foldl (flip (:)) [] 

myReverse'' :: [a] -> [a]
myReverse'' [] = []
myReverse'' (x:xs) = myReverse xs ++  [x]

--5.
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f xs = foldr (\x b -> f x : b) [] xs

myMap' :: (a -> b) -> [a] -> [b]
myMap' _ [] = []
myMap' f xs = foldr ((:) . f) [] xs

myMap'' :: (a -> b) -> [a] -> [b]
myMap'' _ [] = []
myMap'' f (x:xs) = f x : myMap f xs

--6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = foldr (\x b -> if f x then x : b else b) [] xs

myFilterPl :: (a -> Bool) -> [a] -> [a]
myFilterPl f = foldr (\x b -> if f x then x : b else b) [] 

myFilter' :: (a -> Bool) -> [a] -> [a]
myFilter' _ [] = []
myFilter' f (x:xs) =  (if f x then  x : myFilter f xs else  myFilter f xs)

--7.
squish :: [[a]] -> [a]
squish xs = foldr (++) [] xs

squishPl :: [[a]] -> [a]
squishPl = foldr (++) []

--8.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f xs = foldr ((++) . f) [] xs

squishMapPl :: (a -> [b]) -> [a] -> [b]
squishMapPl f = foldr ((++) . f) []

--9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

--10.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f xs = foldr (\x b -> if f x b == GT then x else b) (last xs) xs

myMaximumBy'' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy'' _ [] = undefined
myMaximumBy'' f (x:xs) = foldl (\y b -> if f y b == GT then y else b) x xs

myMaximumBy' :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy' f xs = foldl (\x b -> if f x b == GT then x else b) (head xs) xs
             
--11.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f xs = foldl (\x b -> if f x b == LT then x else b) (head xs) xs