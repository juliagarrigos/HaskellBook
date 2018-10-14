-- 10.5 Fold left --
import Data.Char

-- Functions to print fold associativity 
f :: [Char] -> [Char] -> [Char]
f x y = concat ["(",x,"+",y,")"]

showFoldlA :: [Char]
showFoldlA = foldl f "0" (map show [1..3])

showFoldrA :: [Char]
showFoldrA = foldr f "0" (map show [1..3])

-- Showing fold intermediate stages

showFoldrStages :: [Integer]
showFoldrStages = scanr (+) 0 [1..3]

showFoldlStages :: [Integer]
showFoldlStages = scanl (+) 0 [1..3]


-- Exercises: Understanding Folds

-- foldl (flip (*)) 1 [1..3]
-- f = flip (*)

-- foldl f 1 [1..3]
-- (((1 f 1) f 2) f 3)
-- ((1 f 2) f 3)
-- (2 f 3)
-- 6

-- a) foldr (++) ["woot", "WOOT", "woot"]
g = foldr (++) "" ["woot", "WOOT", "woot"]
-- b) foldr max [] "fear is the little death"
h = foldr max 'a' "fear is the little death"
-- c) foldr and True [False, True]
i = foldr (&&) True [False, True]
-- d)  Can it ever return a different answer? No
j = foldr (||) True [False, True]
-- e) foldl ((++) . show) "" [1..5]
k = foldl (flip $ (++) . show) "" [1..5]
-- f) foldr const 'a' [1..5]
l = foldr (flip const) 'a' [1..5]
-- g) foldr const 0 "tacos"
m = foldr const 'a' "tacos"
-- h) foldl (flip const) 0 "burritos"
n =  foldl const 0 "burritos"
-- i) foldl (flip const) 'z' [1..5]
o = foldl (flip const) 1 [1..5]