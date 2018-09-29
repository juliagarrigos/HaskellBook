module Exercises where 

import Data.List

-- Does it typecheck?

x :: Int -> Int
x blah = blah + 20

printIt :: IO ()
printIt = putStrLn (show (x 45))

-- 1. 
data Person = Person Bool deriving Show

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2. 
data Mood = Blah | Woot deriving (Eq, Show, Ord)

settleDown x = if x == Woot
               then Blah
               else x

-- 4.
type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show) -- Eq is not needed for it to work

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

-- Given a datatype declaration, what can we do?

data Rocks = Rocks String deriving (Eq, Show, Ord)
data Yeah = Yeah Bool deriving (Eq, Show, Ord)
data Papu = Papu Rocks Yeah deriving (Eq, Show, Ord)

-- 1.
phew = Papu (Rocks "chases") (Yeah True)

-- 2.
truth = Papu (Rocks "chomskydoz") (Yeah True)

-- 3.
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4.
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

-- Match the types

-- 1.
-- Doesn't work because it is not possible to put a Num in a variable of type a because it is not of type a anymore
--i :: a
i :: Num a => a
i = 1

-- Same would apply to
--n :: Num a => a
--n = 8 :: Integer

-- 2.
-- a)
f :: Float
-- b) Would work with Fractional instead of Num
--f :: Num a => a 

f = 1.0

-- 3.
-- a) 
--f :: Float
-- b) 
g :: Fractional a => a

g = 1.0

-- 4.
-- a) 
--h :: Float
-- b) 
h :: RealFrac a => a
h = 1.0

-- 5. 
-- a) 
--freud :: a -> a
-- b) 
freud :: Ord a => a -> a
freud x = x


-- 6. 
-- a) 
--freud' :: a -> a
-- b) 
freud' :: Int -> Int
freud' x = x

-- 7. 
myX = 1 :: Int
-- a) 
sigmund :: Int -> Int
-- b) It doesn't work
--sigmund :: a -> a 

sigmund x = myX

-- 8. 
myX' = 1 :: Int
-- a) 
sigmund' :: Int -> Int
-- b) It doesn't work because Num is more polymorphic than Int
--sigmund' :: Num a => a -> a

sigmund' x = myX'

-- 9. 
-- a)
--jung :: Ord a => [a] -> a
-- b) Works because Int is a subclass of Ord
jung :: [Int] -> Int

jung xs = head (sort xs)

-- 10.
-- a) 
--young :: [Char] -> Char
--b) Works because sort expects a list of Ord
young :: Ord a => [a] -> a

young xs = head (sort xs)

--11. 
mySort :: [Char] -> [Char]
mySort = sort
--a) 
signifier :: [Char] -> Char
--b) It doesn't work because Ord a => [a] is more polymorphic than [Char] which is expected by mySort
--signifier :: Ord a => [a] -> a

signifier xs = head (mySort xs)

-- Type-Kwon-Do Two: Electric Typealoo

--1. 
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk aToB a b = (aToB a) == b

--2. 
arith :: Num b => (a -> b) -> Integer -> a -> b
arith aToB someInt a =  (aToB a)  + (fromInteger someInt)