module Example where

triple :: Integer -> Integer
triple x = x * 3

trip = (\x -> x * 3) :: Integer -> Integer

-- Exercises: Grab Bag

-- 1.
mTh x y z = x * y * z
mTh' x y = \z -> x * y * z
mTh'' x = \y -> \z -> x * y * z
mThhh = \x -> \y -> \z -> x * y * z

-- 3.

-- a) 
addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    where f = \n -> n + 1

-- b) 
addFive x y = (if x > y then y else x) + 5

addFive' = \x -> \y -> (if x > y then y else x) + 5

-- c)
mflip f = \x -> \y -> f y x

mflip' f x y = f y x