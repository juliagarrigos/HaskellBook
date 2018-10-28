-- 10.9 Scans --

-- Fibbonacci

fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs
fibsN x = fibs !! x

-- Scans Exercises

--1.
fibs20 = take 20 (1 : scanl (+) 1 fibs)

--2.
fibsfilter = takeWhile (<100) fibs

--3.
fact = scanl (\x b -> x * (b+1)) 1 fact

--Pending