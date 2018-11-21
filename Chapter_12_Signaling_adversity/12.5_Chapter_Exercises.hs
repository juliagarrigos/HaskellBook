-- 12.5 Chapter Exercises --

import Data.Char
import Data.List

-- Determine the kinds

--1. What is the kind of a? * -> *
id' :: a -> a
id' = undefined

--2.What are the kinds of a and f? a = * , f = * -> * 
r :: a -> f a
r = undefined

-- String processing

--1.
notThe :: String -> Maybe String
notThe word 
    | word == "the" = Nothing
    | otherwise  = Just word

testNotThe :: Bool
testNotThe = notThe "blahtheblah" == Just "blahtheblah" && notThe "the" == Nothing

replaceThe :: String -> String
replaceThe text = unwords( map f (words text) )
    where f word = case notThe word of
            Nothing -> "a" 
            otherwise -> word 

testReplaceThe :: IO()
testReplaceThe = if result == "a cow loves us" 
        then print "It worked"
        else print ("It didn't worked, the result was " ++ result)
        where result = replaceThe "the cow loves us"

startsWithVowel :: String -> Bool
startsWithVowel [] = False
startsWithVowel (x:xs) = elem (toLower x) "aeiou"

--2.
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel text = go (words text) 0
    where go :: [String] -> Integer -> Integer
          go [] counter = counter    
          go (x:xs) counter = if x == "the" && (startsWithVowel $ unwords xs) then go xs (counter + 1) else go xs counter

testCountTheBeforeVowel :: Bool
testCountTheBeforeVowel = countTheBeforeVowel "the cow" == 0 && countTheBeforeVowel "the evil cow" == 1

--3.
countVowels :: String -> Integer
countVowels text = countVowels text 0
    where countVowels [] counter = counter 
          countVowels (x:xs) counter = if isVowel x 
                                then countVowels xs (counter + 1)
                                else countVowels xs counter

isVowel :: Char -> Bool
isVowel c = elem c "aeiou"

testCountVowels :: Bool
testCountVowels = countVowels "the cow" == 2 && countVowels "Mikolajczak" == 4

-- Validate the word

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord word = if countVowels > countConsonants then Nothing else Just $ Word' word
            where countVowels = length $ filter isVowel word
                  countConsonants = length $ filter (\x -> isVowel x == False) word

testMkWord :: Bool                  
testMkWord = (mkWord "aaadfeeoi") == Nothing && (mkWord "English") == Just (Word' "English")

-- It’s only Natural

data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = (natToInteger n) + 1

testNatToInteger :: Bool
testNatToInteger = natToInteger Zero == 0 && natToInteger (Succ Zero) == 1 && natToInteger (Succ (Succ Zero)) == 2

integerToNat :: Integer -> Maybe Nat
integerToNat n
    | n < 0 = Nothing
    | otherwise = Just (toNat n)
    where toNat num 
            | num == 0 = Zero
            | num > 0  = Succ (toNat (num - 1))

testIntegerToNat :: Bool
testIntegerToNat = integerToNat 0 == Just Zero 
        && integerToNat 1 == Just (Succ Zero) 
        && integerToNat 2 == Just (Succ (Succ Zero)) 
        && integerToNat (-1) == Nothing

-- Small library for Maybe
--1.
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust (Nothing) = False

testIsJust = isJust (Just 1) == True && isJust Nothing == False

isNothing :: Maybe a -> Bool
isNothing = not . isJust

testIsNothing = isNothing (Just 1) == False && isNothing Nothing == True

--2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee b aToB mayA = case mayA of
        Nothing -> b
        (Just a) -> aToB a 

testMayybee = mayybee 0 (+1) Nothing == 0 && mayybee 0 (+1) (Just 1) == 2

--3.
fromMaybe :: a -> Maybe a -> a
fromMaybe a mayA = mayybee a id mayA

testFromMaybe = fromMaybe 0 Nothing == 0 && fromMaybe 0 (Just 1) == 1

--4.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

--5.
catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs) = case x of
        Just n -> n : catMaybes xs
        Nothing -> catMaybes xs

--6.
flipMaybe :: Eq a=> [Maybe a] -> Maybe [a]
flipMaybe list = if elem Nothing list 
        then Nothing 
        else Just (catMaybes list)

-- Small library for Either

--1.
lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' list = foldr f [] list
        where f :: Either a b -> [a] -> [a]
              f e acc = case e of
                        (Left l) -> l : acc
                        otherwise -> acc

testLefts = lefts' [Left 3, Left 8, Right "hello", Right "world", Left 9] == [3,8,9]

--2.
rights' :: [Either a b] -> [b]
rights' [] = []
rights' list = foldr f [] list
        where f :: Either a b -> [b] -> [b]
              f e acc = case e of
                (Right l) -> l : acc
                otherwise -> acc

testRights = rights' [Left 3, Left 8, Right "hello", Right "world", Left 9] == ["hello","world"]

--3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' [] = ([],[])
partitionEithers' list = foldr f ([],[]) list
                where f :: Either a b -> ([a],[b]) -> ([a],[b])
                      f e acc = case e of
                        (Left l) -> (l: (fst acc), snd acc)
                        (Right r) -> (fst acc, r : (snd acc))

testPartitionEithers = partitionEithers' [Left 3, Left 8, Right "hello", Right "world", Left 9] == ([3,8,9],  ["hello","world"] )

--4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f e = case e of
                (Right r) -> Just (f r)
                otherwise -> Nothing

testEitherMaybe = eitherMaybe' (+1) (Left 4) == Nothing && eitherMaybe' (+1) (Right 5) == Just 6

--5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fl _ (Left l) = fl l
either' _ fr (Right r) = fr r

testEither = either' (+1) (+2) (Left 5) == 6 && either' (+1) (+2) (Right 8) == 10

--6.
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' _ (Left _) = Nothing
eitherMaybe'' f (Right r) = Just (either' id f (Right r))

testEitherMaybe'' = eitherMaybe'' (+1) (Left 4) == Nothing && eitherMaybe'' (+1) (Right 5) == Just 6

-- Why bother?

mehSum :: Num a => [a] -> a
mehSum xs = go 0 xs
        where   go :: Num a => a -> [a] -> a
                go n [] = n
                go n (x:xs) = (go (n+x) xs)

niceSum :: Num a => [a] -> a
niceSum = foldl' (+) 0

mehProduct :: Num a => [a] -> a
mehProduct xs = go 1 xs
        where   go :: Num a => a -> [a] -> a
                go n [] = n
                go n (x:xs) = (go (n*x) xs)

niceProduct :: Num a => [a] -> a
niceProduct = foldl' (*) 1

mehConcat :: [[a]] -> [a]
mehConcat xs = go [] xs
        where   go :: [a] -> [[a]] -> [a]
                go xs' [] = xs'
                go xs' (x:xs) = (go (xs' ++ x) xs)

niceConcat :: [[a]] -> [a]
niceConcat = foldr (++) []      

-- Write your own iterate and unfoldr
--1.
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

testMyIterate = take 5 (myIterate (+1) 1) == [1,2,3,4,5]

--2.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
         (Just(x,y)) -> x : myUnfoldr f y
         Nothing -> []

testMyUnfoldr = take 5 (myUnfoldr (\b ->Just(b,b+1)) 1) == [1,2,3,4,5]

--3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = unfoldr (\b-> Just (b, f b)) x

testBetterIterate = take 5 (betterIterate (+1) 1) == [1,2,3,4,5]

-- Finally something other than a list!

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

--1.
unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f a = case f a of
        Nothing -> Leaf
        Just (x,y,z) -> Node (unfold f x) y (unfold f z)

-- --2.
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold f 0
        where f b = if b < 0 || b == n then Nothing else Just (b+1, b, b+1)

testTreeBuild = treeBuild 3 == Node (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf)) 0 (Node (Node Leaf 2 Leaf) 1 (Node Leaf 2 Leaf))

