-- Chapter Exercises

-- Multiple choice
--2.
f :: Char -> String
f = undefined

g :: String -> [String]
g = undefined

h :: Char -> [String]
h = g . f

--3.
i :: Ord a => a -> a -> Bool
i = undefined

--5.
j :: a -> a
j x = x

-- Let’s write code
--1.
tensDigit :: Integral a => a -> a
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10  

tensDigit' :: Integral a => a -> a
tensDigit' x = d
    where xLast = fst $ x `divMod` 10
          d = snd $ xLast `divMod` 10

tensDigit'' :: Integral a => a -> a
tensDigit'' x = d
    where (xLast, _) = x `divMod` 10
          (_,d) =  xLast `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d 
    where (xLast, _) = x `divMod` 100
          (_,d) =  xLast `divMod` 10  

--2.
foldBool :: a -> a -> Bool -> a
foldBool x y z = case z of
    False -> x
    True -> y 

foldBool' :: a -> a -> Bool -> a
foldBool' x y z  
    | z = y 
    | otherwise = x

--3.
l :: (a -> b) -> (a, c) -> (b, c)
l aToB (a,c) = (aToB a, c)

--4.

-- Arith4 
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

main = do
    print (roundTrip 4)
    print (id 4)

--5.
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show 

--6.
roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' a = read (show a)


blah x = x
blah' = id
addAndDrop x y = x + 1
addAndDrop' = (+1)
reverseMkTuple a b = (b, a)
reverseMkTuple' = flip (,)
reverseTuple (a, b) = (b, a)
reverseTuple' = flip