module WordNumber where

import           Data.List                      ( intersperse )
import           Data.Char                      ( toUpper )

digitToWord :: Int -> String
digitToWord n = case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    3 -> "three"
    4 -> "four"
    5 -> "five"
    6 -> "six"
    7 -> "seven"
    8 -> "eight"
    9 -> "nine"
    _ -> ""

digits :: Int -> [Int]
digits n = go n []
  where
    go :: Int -> [Int] -> [Int]
    go 0  l = l
    go n' l = go (div n' 10) l ++ [mod n' 10]

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs
