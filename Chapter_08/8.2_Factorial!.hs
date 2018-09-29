-- 8.2 Factorial! --

fourFactorial :: Integer
fourFactorial = 4 * 3 * 2 * 1

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

inc :: Num a => a -> a
inc = (+1)
three' = (inc . inc . inc) 0

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = 1 + (incTimes (times - 1) n)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' 0 n = n
incTimes' times n = incTimes (times - 1) (n + 1)

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
--applyTimes n f b = f (applyTimes (n-1) f b)
--applyTimes n f b = f (applyTimes (n-1) f b)
applyTimes n f b = f . applyTimes (n-1) f $ b
--applyTimes n f b = applyTimes (n-1) f (f b)

incTimes'' :: (Eq a, Num a) => a -> a -> a
incTimes'' times n = applyTimes times (+1) n

-- Intermission: Exercise
-- applyTimes 5 (+1) 5
