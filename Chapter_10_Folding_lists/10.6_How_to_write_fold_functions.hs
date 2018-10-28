-- 10.6 How to write fold functions --

import Data.Time

concat3 xs = foldr ((++) . take 3) "" xs
concat3' xs = foldr (\a b -> take 3 a ++ b) "" xs

-- Exercises: Database Processing
data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime 
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [ 
    DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbString "Hello, world!"
    , DbNumber 9001
    , DbNumber 90
    , DbNumber 901
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

--1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate [] = []
filterDbDate xs = foldr f [] xs
    where f :: DatabaseItem -> [UTCTime] -> [UTCTime]
          f (DbDate a) b = a : b
          f _ b = b

--2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = foldr f [] xs
    where f :: DatabaseItem -> [Integer] -> [Integer]
          f (DbNumber a) b = a : b
          f _ b = b 

--3.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr f (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)) xs
    where f (DbDate a) b = max a b
          f _ b = b

mostRecent' :: [DatabaseItem] -> UTCTime
mostRecent' = foldr max (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)) . filterDbDate 

--4.
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

--5.
avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral (sumDb xs) / fromIntegral (length $ filterDbNumber xs)



