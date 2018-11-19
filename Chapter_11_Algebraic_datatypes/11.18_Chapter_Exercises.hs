-- 11.18 Chapter Exercises

import Data.Char
import Data.List

-- Ciphers

-- Caesar Cipher
caesarRight :: String -> Int -> String
caesarRight [] _ = []
caesarRight (x:xs) s = shiftChar x s : caesarRight xs s

shiftChar :: Char -> Int -> Char
shiftChar c s = chr $ mod (ord c - ord 'a' + s ) 26 +  ord 'a'

unshiftChar :: Char -> Int -> Char
unshiftChar c s = chr $ mod (ord c - ord 'a' - s ) 26 +  ord 'a'

uncaesarRight :: String -> Int -> String
uncaesarRight [] _ = []
uncaesarRight (x:xs) s = unshiftChar x s : uncaesarRight xs s

testingCaesar = uncaesarRight (caesarRight "helloworld" 7) 7 == "helloworld"

-- Vigenère cipher

alphabetIndex :: Char -> Int
alphabetIndex x = ord x - ord 'a'

encode :: (Char, Char) -> Char
encode (x, y) = shiftChar x (alphabetIndex y)

decode :: (Char, Char) -> Char
decode (x, y) = unshiftChar x (alphabetIndex y)

vigenere :: String -> String -> String
vigenere message key = map encode $ zipWith (,) message (concat $ repeat key)

unvigenere :: String -> String -> String
unvigenere message key = map decode $ zipWith (,) message (concat $ repeat key)

testingVigenere :: IO ()
testingVigenere = if encodingResult == "ecmvcfkowqmeo" && decodingResult == message 
    then putStrLn "It worked!"
    else putStrLn ("It didn't worked, result: " ++ encodingResult ++ " " ++ decodingResult)
    where 
        message = "secretmessage"
        key = "mykey"
        encodingResult = vigenere message key
        decodingResult = unvigenere encodingResult key

-- As-patterns
f :: Show a => (a, b) -> IO (a, b)
f t@(a, _) = do
            print a
            return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

--1. This should return True if (and only if) all the values in the first 
--list appear in the second list, though they need not be contiguous. 
--Remember that the sub-sequence has to be in the original order
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf xxs@(x:xs) (y:ys) = if x == y 
    then isSubseqOf xs ys
    else isSubseqOf xxs ys

testIsSubseqOf :: Bool
testIsSubseqOf =  (isSubseqOf "blah" "blahwoot" == True ) 
    && (isSubseqOf "blah" "wootblah" == True) 
    && (isSubseqOf "blah" "wboloath" == True) 
    && (isSubseqOf "blah" "wootbla" == False) 
    && (isSubseqOf "blah" "halbwoot" == False) 

-- 2. Split a sentence into words, then tuple each word with the capitalized form of each.

capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords s = map toTuple (words s) where 
    toTuple [] = error "String should not be empty"
    toTuple xxs@(x:xs) = (xxs, (toUpper x): xs)

testCapitalizeWords :: Bool
testCapitalizeWords  = capitalizeWords "hello world" == [("hello", "Hello"), ("world", "World")]

-- Language exercises
--1. Write a function that capitalizes a word.

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

testCapitalizeWord :: Bool
testCapitalizeWord = capitalizeWord "Chortle" == "Chortle" && capitalizeWord "chortle" == "Chortle"

-- 2. Write a function that capitalizes sentences in a paragraph. Recognize when a new 
--sentence has begun by checking for periods. Reuse the capitalizeWord function. 

capitalizeParagraph :: String -> String
capitalizeParagraph p = go $ capitalizeWord p
    where 
        go :: String -> String
        go [] = [] 
        go ('.':' ':xs) = ". " ++ go (capitalizeWord xs)
        go (x:xs) = x : go xs
    
testCapitalizeParagraph :: Bool
testCapitalizeParagraph = capitalizeParagraph "blah. woot ha." == "Blah. Woot ha."

-- Phone exercise

