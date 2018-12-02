module Tests where

import Test.Hspec
import Test.QuickCheck
import WordNumber(digitToWord, digits, wordNumber, capitalizeWord)
import Data.List (sort)
import Ciphers

-- 14.7 Chapter Exercises --

-- Validating numbers into words

wordNumberTests :: IO ()
wordNumberTests = hspec $ do
    describe "digitToWord" $ do
        it "returns zero for 0" $ digitToWord 0 `shouldBe` "zero"
        it "returns one for 1" $ digitToWord 1 `shouldBe` "one"
    describe "digits" $ do
        it "returns [1] for 1" $ digits 1 `shouldBe` [1]
        it "returns [1, 0, 0] for 100" $ digits 100 `shouldBe` [1, 0, 0]
    describe "wordNumber" $ do
        it "one-zero-zero given 100" $ wordNumber 100 `shouldBe` "one-zero-zero"
        it "nine-zero-zero-one for 9001" $ wordNumber 9001 `shouldBe` "nine-zero-zero-one"

-- Using QuickCheck

--1.
half :: Fractional a => a -> a
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (* 2) . half

genNumerator :: Gen Double
genNumerator = arbitrary `suchThat` (/= 0)

prop_half :: Property
prop_half = forAll genNumerator (\x -> (half x) * 2 == x)

prop_halfIdentity :: Property
prop_halfIdentity = forAll genNumerator (\x -> (halfIdentity x) == x)

halfTest :: IO ()
halfTest = do
    quickCheck prop_half
    quickCheck prop_halfIdentity

--2.

-- for any list you apply sort to
-- this property should hold
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
    where   go _ status@(_, False) = status
            go y (Nothing, t) = (Just y, t)
            go y (Just x, t) = (Just y, x >= y)

genList :: (Arbitrary a, Ord a, Eq a, Show a) => Gen [a]
genList = do
    a <- arbitrary
    b <- arbitrary `suchThat` (/= a)
    c <- arbitrary `suchThat` (`notElem` [a, b])
    d <- arbitrary `suchThat` (`notElem` [a, b, c])
    return [a, b, c, d]

prop_sort :: (Arbitrary a, Ord a, Eq a, Show a) => Gen [a] -> Property
prop_sort gen = forAll gen (\x -> listOrdered (sort x))

sortTest :: IO ()
sortTest = do
    quickCheck (prop_sort (genList :: Gen [Integer]))
    quickCheck (prop_sort (genList :: Gen [Char]))
    quickCheck (prop_sort (genList :: Gen [Float]))

--3.
plusAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
plusAssociative x y z = x + (y + z) == (x + y) + z

genThree :: (Arbitrary a, Eq a, Num a, Show a) => Gen (a, a, a)
genThree = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (a, b, c)

prop_plusAssoc
    :: (Arbitrary a, Eq a, Num a, Show a) => Gen (a, a, a) -> Property
prop_plusAssoc gen = forAll gen (\(x, y, z) -> plusAssociative x y z)

plusAssocTest :: IO ()
plusAssocTest = quickCheck (prop_plusAssoc (genThree :: Gen (Integer, Integer, Integer)))

plusCommutative :: (Num a, Eq a) => a -> a -> Bool
plusCommutative x y = x + y == y + x

genTuple :: (Arbitrary a, Num a, Eq a, Show a) => Gen (a, a)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

prop_plusCom :: (Arbitrary a, Num a, Eq a, Show a) => Gen (a, a) -> Property
prop_plusCom gen = forAll gen (\(x, y) -> plusCommutative x y)

comTest :: IO ()
comTest = do
    quickCheck (prop_plusCom (genTuple :: Gen (Integer, Integer)))
    quickCheck (prop_plusCom (genTuple :: Gen (Float, Float)))
    quickCheck (prop_plusCom (genTuple :: Gen (Double, Double)))

--4.
multAssociative :: (Num a, Eq a) => a -> a -> a -> Bool
multAssociative x y z = x * (y * z) == (x * y) * z

prop_multAssoc
    :: (Arbitrary a, Num a, Eq a, Show a) => Gen (a, a, a) -> Property
prop_multAssoc gen = forAll gen (\(x, y, z) -> multAssociative x y z)

multCommutative :: (Num a, Eq a) => a -> a -> Bool
multCommutative x y = x * y == y * x

prop_multCom :: (Arbitrary a, Num a, Eq a, Show a) => Gen (a, a) -> Property
prop_multCom gen = forAll gen (\(x, y) -> multCommutative x y)

multTest :: IO ()
multTest = do
    quickCheck (prop_multAssoc (genThree :: Gen (Integer, Integer, Integer)))
    quickCheck (prop_multCom (genTuple :: Gen (Integer, Integer)))
    quickCheck (prop_multCom (genTuple :: Gen (Float, Float)))
    quickCheck (prop_multCom (genTuple :: Gen (Double, Double)))

--5.
genDivisonTuple :: Gen (Integer, Integer)
genDivisonTuple = do
    numerator   <- arbitrary
    denominator <- arbitrary `suchThat` (/= 0)
    return (numerator, denominator)

quotRemProperty :: (Eq a, Integral a) => a -> a -> Bool
quotRemProperty x y = (quot x y) * y + (rem x y) == x

quotModProperty :: (Eq a, Integral a) => a -> a -> Bool
quotModProperty x y = (div x y) * y + (mod x y) == x

prop_quotRemProperty :: Property
prop_quotRemProperty = forAll genDivisonTuple (\(x, y) -> quotRemProperty x y)

prop_quotModProperty :: Property
prop_quotModProperty = forAll genDivisonTuple (\(x, y) -> quotModProperty x y)

quotRemTest :: IO ()
quotRemTest = do
    quickCheck prop_quotRemProperty
    quickCheck prop_quotModProperty

--6.

genExpTriple :: Gen (Integer, Integer, Integer)
genExpTriple = do
    a <- arbitrary `suchThat` (> 0)
    b <- arbitrary `suchThat` (> 0)
    c <- arbitrary `suchThat` (> 0)
    return (a, b, c)

prop_expAssoc :: Property
prop_expAssoc = forAll genExpTriple (\(x, y, z) -> x ^ (y ^ z) == (x ^ y) ^ z)

genExpTuple :: Gen (Integer, Integer)
genExpTuple = do
    a <- arbitrary `suchThat` (> 0)
    b <- arbitrary `suchThat` (> 0)
    return (a, b)

prop_expCom :: Property
prop_expCom = forAll genExpTuple (\(x, y) -> x ^ y == y ^ x)

checkExpProperties :: IO ()
checkExpProperties = do
    quickCheck prop_expAssoc
    quickCheck prop_expCom

--7.
prop_reverseList :: (Arbitrary a, Ord a, Eq a, Show a) => Gen [a] -> Property
prop_reverseList gen = forAll gen (\xs -> (reverse . reverse) xs == id xs)

reverseTest :: IO ()
reverseTest = do
    quickCheck (prop_reverseList (genList :: Gen [Integer]))
    quickCheck (prop_reverseList (genList :: Gen [Char]))
    quickCheck (prop_reverseList (genList :: Gen [Float]))

--8.

intGen :: Gen Int
intGen = arbitrary

prop_dollarProperty ::  (Show a, Eq a, Eq b) => (a -> b) -> Gen a -> Property
prop_dollarProperty f gen = forAll gen (\x -> (f $ x) == f x)

dollarTest :: IO ()
dollarTest = do
    quickCheck (prop_dollarProperty (+1) intGen)
    quickCheck (prop_dollarProperty (*5) intGen)

prop_compProperty :: (Show a, Eq a, Eq b, Eq c) => (a -> b) -> (b -> c) -> Gen a  -> Property
prop_compProperty g f gen = forAll gen (\x -> (f . g) x == (\x -> f (g x)) x)

compositionTest :: IO()
compositionTest = do
    quickCheck (prop_compProperty (+1) (\x -> "Number "++ show x) intGen)
    quickCheck (prop_compProperty (*5) (\x -> "Number "++ show x) intGen)

--9.
prop_foldConsConcat :: (Show a, Eq a) => Gen[a] -> Property
prop_foldConsConcat gen = forAll gen (\x -> (foldr (:) x x) == ((++) x x))

foldConsConcatTest :: IO()
foldConsConcatTest = do
    quickCheck (prop_foldConsConcat (genList :: Gen [String]))
    quickCheck (prop_foldConsConcat (genList :: Gen [Int]))

prop_foldConcatConcat :: (Show a, Eq a) => Gen[a] -> Property
prop_foldConcatConcat gen = forAll gen (\x -> (foldr (++) [] [x]) == (concat [x, x]))

foldConcatConcatTest :: IO()
foldConcatConcatTest = do
    quickCheck (prop_foldConcatConcat (genList :: Gen [String]))
    quickCheck (prop_foldConcatConcat (genList :: Gen [Int]))

--10. f n xs = length (take n xs) == n

genListAndNum :: (Arbitrary a, Ord a, Eq a, Show a) => Gen (Int, [a])
genListAndNum = do
    a <- arbitrary
    b <- arbitrary `suchThat` (/= a)
    c <- arbitrary `suchThat` (`notElem` [a, b])
    d <- arbitrary `suchThat` (`notElem` [a, b, c])
    e <- choose (1,4)
    return (e, [a, b, c, d])

prop_takeLength :: (Show a, Eq a) => Gen(Int,[a]) -> Property
prop_takeLength gen = forAll gen (\(n, xs) -> (length (take n xs)) == n)

takeLengthTest :: IO()
takeLengthTest = do
    quickCheck (prop_takeLength (genListAndNum :: Gen (Int, [String])))
    quickCheck (prop_takeLength (genListAndNum :: Gen (Int, [Int])))

--11. f x = (read (show x)) == x

prop_showRead :: (Show a, Read a, Eq a) => Gen a -> Property
prop_showRead gen = forAll gen (\x -> (read (show x)) == x)

showReadTest :: IO()
showReadTest = do
    quickCheck (prop_showRead (arbitrary :: Gen Bool))
    quickCheck (prop_showRead (arbitrary :: Gen Int))

-- Failure
-- square x = x * x
-- squareIdentity = square . sqrt

positiveFloatGen :: Gen Float
positiveFloatGen = (arbitrary :: Gen Float) `suchThat` (>0)

square :: Num a => a -> a
square x = x * x

prop_squareSqrt :: Property
prop_squareSqrt = forAll positiveFloatGen (\x -> (square . sqrt) x == x)

squareSqrtTest :: IO()
squareSqrtTest = do
    quickCheck prop_squareSqrt 
    quickCheck prop_squareSqrt 

-- Idempotence

twice :: (b -> b) -> b -> b
twice f = f . f

fourTimes :: (b -> b) -> b -> b
fourTimes = twice . twice

--1. 
prop_idempotenceCapitalizeWord :: Property
prop_idempotenceCapitalizeWord = forAll (arbitrary :: Gen String) 
    (\x -> capitalizeWord x == twice capitalizeWord x && capitalizeWord x == fourTimes capitalizeWord x)

idempotenceCapitalizeWordTest :: IO()
idempotenceCapitalizeWordTest = quickCheck prop_idempotenceCapitalizeWord

--2. 
prop_idempotenceSort :: (Show a, Eq a, Ord a) => Gen [a] -> Property
prop_idempotenceSort gen = forAll gen (\x -> sort x == twice sort x && sort x == fourTimes sort x)

idempotenceSortTest :: IO()
idempotenceSortTest = do
    quickCheck (prop_idempotenceSort(genList :: Gen [String]))
    quickCheck (prop_idempotenceSort(genList :: Gen [Char]))
    quickCheck (prop_idempotenceSort(genList :: Gen [Float]))

-- Make a Gen random generator for the datatype

-- 1. Equal probabilities for each

data Fool = Fulse | Frue deriving (Eq, Show)

foolGen :: Gen Fool
foolGen = oneof [return $ Fulse, return $ Frue]

-- 2. 2/3s change of Fulse, 1/3 change of Frue

foolGen' :: Gen Fool
foolGen' = frequency [(2, return $ Fulse), (1,return $ Frue)]

-- Hangman testing --> tests added in hangman project

-- Validating ciphers

shiftGen :: Gen Int
shiftGen = elements[1..20]

wordGen :: Gen String
wordGen = listOf (elements ['a'..'z'])

caesarGen :: Gen (Int, String)
caesarGen = do
    shift <- shiftGen 
    word <- wordGen
    return (shift, word)

caesarCipherProperty :: Property
caesarCipherProperty = forAll caesarGen (\(shift, word) -> uncaesarRight (caesarRight word shift) shift == word)

vigenreGen :: Gen (String, String)
vigenreGen = do
    key <- wordGen 
    word <- wordGen
    return (key, word)

vigenreCipherProperty :: Property
vigenreCipherProperty = forAll vigenreGen (\(key, word) -> unvigenere (vigenere word key) key == word)

ciphersTest :: IO()
ciphersTest = do
    quickCheck caesarCipherProperty
    quickCheck vigenreCipherProperty


