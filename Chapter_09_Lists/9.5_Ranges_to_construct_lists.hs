-- 9.5 Using ranges to construct lists --

-- Exercise: EnumFromTo

eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False , True]
eftBool True True = [True]
eftBool False False = [False , False]
eftBool True False = [] 

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd GT GT = [GT]
eftOrd GT _ = [GT]
eftOrd a b = eft a b

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

eft :: (Enum a, Ord a) => a -> a -> [a]
eft a b = go a []
    where go n r 
            | n == b = r ++ [n]
            | otherwise = go (succ n) (r ++ [n])
