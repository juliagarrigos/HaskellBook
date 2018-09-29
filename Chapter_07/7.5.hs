module Example where

funcZ x = 
    case x + 1 == 1 of
        True -> "AWESOME"
        False -> "wut"

pal xs = 
    case xs == reverse xs of
        True -> "PALINDROME"
        False -> "NO PALINDROME"

pal' xs =
    case y of
        True -> "yes"
        False -> "no"
        where y = xs == reverse xs

-- GreetIfCool3 

greetIfCool :: String -> IO ()
greetIfCool coolness =
    case cool of
        True -> putStrLn "eyyyyy. What's shakin'?"
        False -> putStrLn "pshhhh."
        where cool = coolness == "downright frosty yo"

-- Exercises: Case Practice

-- 1. 
functionC x y = if (x > y) then x else y

functionC' x y = case x > y of
    True -> x
    False -> y

-- 2. 
ifEvenAdd2 n = if even n then (n+2) else n

ifEvenAdd2' n = case even n of
    True -> n + 2
    False -> n

-- 3.
nums x =
    case compare x 0 of
    LT -> -1
    GT -> 1
    EQ -> 0