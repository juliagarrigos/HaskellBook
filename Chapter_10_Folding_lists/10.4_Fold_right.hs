-- 10.4 Fold right --
myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs