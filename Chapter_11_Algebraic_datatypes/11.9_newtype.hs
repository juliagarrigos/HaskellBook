-- 11.9 newtype --

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

tooManyGoats :: Int -> Bool
tooManyGoats n = n > 42

newtype Goats = Goats Int deriving (Eq, Show)
newtype Cows = Cows Int deriving (Eq, Show)

tooManyGoats' :: Goats -> Bool
tooManyGoats' (Goats n) = n > 42

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

instance TooMany Goats where
    tooMany (Goats n) = n > 43

newtype Goats' = Goats' Int deriving (Eq, Show, TooMany)

-- Exercises: Logic Goats

--1.
class TooMany' a where
    tooMany' :: a -> Bool

instance TooMany' (Int, String) where
    tooMany' (n, _) = n > 50

--2.
instance TooMany' (Int, Int) where
    tooMany' (x, y) = (x + y) > 50

--3.

instance (Num a, TooMany' a, Ord a) => TooMany' (a, a) where
    tooMany' (x, y) = (x + y) > 50