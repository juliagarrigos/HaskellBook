-- 15.12 Better living through QuickCheck--

import Data.Monoid
import Test.QuickCheck
import Control.Monad

-- Validating associativity with QuickCheck

-- Function to check associativity property
asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c = a <> (b <> c) == (a <> b) <> c

-- Function to check associativity property for monoids
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

assocTest :: IO()
assocTest = quickCheck (monoidAssoc :: String -> String -> String -> Bool)

-- Testing left and right identity

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = (mempty <> x) == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = (x <> mempty) == x

identityTest :: IO()
identityTest = do
    quickCheck (monoidLeftIdentity :: String -> Bool)
    quickCheck (monoidRightIdentity :: String -> Bool)

-- Testing QuickCheck’s patience (QuickCheck proves that Bull is not a Monoid)
data Bull = Fools | Twoo deriving (Eq, Show)

instance Arbitrary Bull where
    arbitrary = frequency [ (1, return Fools), (1, return Twoo) ]

instance Semigroup Bull where
    (<>) = mappend

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools

type BullMappend = Bull -> Bull -> Bull -> Bool

bullTest :: IO ()
bullTest = do
    quickCheck (monoidAssoc :: BullMappend)
    quickCheck (monoidLeftIdentity :: Bull -> Bool)
    quickCheck (monoidRightIdentity :: Bull -> Bool)

-- Exercise: Maybe Another Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

newtype First' a = First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
    (<>) f1@(First' (Only x)) _ =  f1 
    (<>) _ f2 =  f2

instance  Monoid (First' a) where
    mempty = First' Nada

firstGen :: Arbitrary a => Gen (First' a)
firstGen =  do
            a <- arbitrary
            oneof [return (First' Nada), return (First' (Only a))]

instance Arbitrary a => Arbitrary (First' a) where
    arbitrary = firstGen

firstMappend :: Semigroup a => First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

maybeTest :: IO ()
maybeTest = do
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)


