module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True 

main' :: IO ()
main' = hspec $ do
  describe "Addition" $ do
    it "1 + 1 is greater than 1" $ do
      (1 + 1) > 1 `shouldBe` True -- hspec
    it "2 + 2 is equal to 4" $ do
      2 + 2 `shouldBe` 4 -- hspec
    it "x + 1 is always\
      \ greater than x" $ do
      property $ \x -> x + 1 > (x :: Int) -- QuickCheck

-- Using QuickCheck without Hspec
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

-- Gens
oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
          a <- arbitrary
          b <- arbitrary
          return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
              a <- arbitrary
              b <- arbitrary
              c <- arbitrary
              return (a, b, c)

              
genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
            a <- arbitrary
            b <- arbitrary
            elements [Left a, Right b]

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
            a <- arbitrary
            elements [Nothing, Just a]

-- What QuickCheck does so you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
            a <- arbitrary
            frequency [ (1, return Nothing), (3, return (Just a))]


