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
    , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
    ]

--1.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = undefined
