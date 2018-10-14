import Data.Bool
-- 9.9 Transforming lists of values --

--Exercises: More Bottoms

--4.

itIsMystery xs = map (\x -> elem x "aeiou") xs

--6.
--map (\x -> if x == 3 then (-x) else (x)) [1..10]

newMap :: Integral a => [a]
newMap = map (\x -> (bool (x) (-x) (x == 3))) [1..10]
