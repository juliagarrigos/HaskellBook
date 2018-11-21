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

data DaPhone = DaPhone { buttons:: [Button]}

type Digit = Char -- "1234567890*#"
type Presses = Int
data Button = Button {digit::Digit, characters::[Char]}
daPhone :: DaPhone
daPhone = DaPhone {buttons = [
    Button '1' ['1'], 
    Button '2' ['a','b','c','2'], 
    Button '3' ['d','e','f','3'], 
    Button '4' ['g','h','i','4'], 
    Button '5' ['j','k','l','5'], 
    Button '6' ['m','n','o','6'], 
    Button '7' ['p','q','r','s', '7'], 
    Button '8' ['t','u','v','8'], 
    Button '9' ['w','x','y','z','9'], 
    Button '0' [' ','0'], 
    Button '*' ['^'], 
    Button '#' ['.',','] ]}

indexOfCharInButton :: Button -> Char -> Int
indexOfCharInButton (Button _ chars) charToFind = case elemIndex (toLower charToFind) chars of
    Just position -> position
    Nothing -> error ("error2 char not found "++[charToFind])

buttonForChar :: DaPhone -> Char -> Button
buttonForChar phone charToFind = case find (isButton charToFind) (buttons phone) of
    Just button -> button 
    Nothing -> error ("error1 button not found for char "++ [charToFind] ++".")
    where isButton char (Button _ chars) = if isUpper char
            then elem (toLower char) chars
            else elem char chars

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps phone charToFind =  if isUpper charToFind 
                                then [('*',1), (buttonDigit, position + 1)] 
                                else [(buttonDigit, position + 1)]
                                where position = indexOfCharInButton button charToFind
                                      button = buttonForChar phone charToFind 
                                      buttonDigit = digit button

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead phone message = concat $ map (reverseTaps phone) message

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps list = foldr (+) 0 (map (\ (_,p) -> p) list)

testFingerTaps = (fingerTaps $ cellPhonesDead daPhone "Lol ya. Abba") == 24
testCellPhonesDead = cellPhonesDead daPhone "Lol ya. Abba" == [('*',1),('5',3),('6',3),('5',3),('0',1),('9',3),('2',1),('#',1),('0',1),('*',1),('2',1),('2',2),('2',2),('2',1)]

mostPopularLetter :: DaPhone -> String -> String
mostPopularLetter phone message = "The most popular letter is "++[char]++ " with a cost of "++show cost
        where (char, times) =  head letters 
              letters = filter (\(x,_) -> x /= ' ') (sortBy (\(x1,y1) (x2,y2) -> compare y2 y1) (allCharOcurrences message)  )
              cost = fingerTaps(reverseTaps phone char) * times

charOcurrences :: String -> Char -> Int
charOcurrences message char = foldr f 0 message
    where f :: Char -> Int -> Int
          f currentChar acc = if currentChar == char 
                              then acc + 1
                              else acc

allCharOcurrences :: String -> [(Char,Int)]
allCharOcurrences message = map f (removeDuplicates message)
            where f char = (char, charOcurrences message char)

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates list = foldr f [] list
            where f currentElement acc = if elem currentElement acc then acc else currentElement : acc 

convo :: [String]
convo = ["Wanna play 20 questions",
        "Ya",
        "U 1st haha",
        "Lol ok. Have u ever tasted alcohol",
        "Lol ya",
        "Wow ur cool haha. Ur turn",
        "Ok. Do u think I am pretty Lol",
        "Lol ya",
        "Just making sure rofl ur turn"]

coolestLtr :: [String] -> String
coolestLtr phrases = mostPopularLetter daPhone (concat phrases)

coolestWord :: [String] -> String
coolestWord phrases = foldr f "" allWords
    where f :: String -> String -> String
          f a b = if countOcurrences a > countOcurrences b then a else b
          countOcurrences word = length(filter (==word) allWords)
          allWords = words (concat phrases)

-- Hutton’s Razor

data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2

testEval = eval (Add (Lit 1) (Lit 9001)) == 9002

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2

testPrinter = if result == "1 + 9001" 
    then print "It worked!"
    else print ("Wrong, result: " ++ show result)
    where result = printExpr (Add (Lit 1) (Lit 9001))

testPrinter2 = if result == "1 + 9001 + 1 + 20001" 
    then print "It worked!"
    else print ("Wrong, result: " ++ show result)
    where result = printExpr a3
          a1 = Add (Lit 9001) (Lit 1)
          a2 = Add a1 (Lit 20001)
          a3 = Add (Lit 1) a2