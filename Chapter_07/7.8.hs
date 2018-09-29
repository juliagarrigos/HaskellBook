-- Function composition

--(.) :: (b -> c) -> (a -> b) -> a -> c

f :: (Num a, Enum a) => [a] 
f = take 5 . reverse $ [1..10]

g :: (Num a, Enum a) => a -> [a] 
g x = take 5 . enumFrom $ x

h :: Int -> Int -> [Int]
h x y = take x . filter odd . enumFrom $ y

h' :: Int -> Int -> [Int]
h' x y = take x (filter odd  (enumFrom $ y))