module Example1 where

add :: Num a => a -> a -> a
add x y = x + y

addWeird :: (Num a, Ord a) => a -> a -> a
addWeird x y =
    if x > 1
    then x + y
    else x