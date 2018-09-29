module Example where

myNumber :: Integer
myNumber = 1

myValue f = myNumber

----

myNum :: Num a => a
myNum = 1

myVal :: Num a => a -> a
myVal f = f + myNum

stillAFunction :: [a] -> [a] -> [a] -> [a]
stillAFunction a b c = a ++ b ++ c

----

addOne :: Integer -> Integer
addOne x = x + 1

bindExp :: Integer -> String
bindExp x =
    let y = 5 in 
        "the integer was: " ++ show x ++ " and y was: " ++ show y


-- bindExp' :: Integer -> String
-- bindExp' x =
--     let z = y + x in
--     let y = 5 in
--         "the integer was: "
--         ++ show x ++ " and y was: "
--         ++ show y ++ " and z was: "
--         ++ show z

bindExp'' :: Integer -> String
bindExp'' x =
        let x = 10; y = 5 in
        "the integer was: " ++ show x
        ++ " and y was: " ++ show y