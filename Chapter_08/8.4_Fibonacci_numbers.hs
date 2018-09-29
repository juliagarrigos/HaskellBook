-- 8.4 Fibonacci numbers --



-- 1, 1, 2, 3, 5, 8, 13, 21, 34...
-- 
f :: Integer -> Integer -> Integer -> Integer
f 0 n m = m
f t n m = f (t-1) m (n+m)

fibonacci' :: Integer -> Integer
fibonacci' t = f t 0 1 

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)
