-- 15.15 Chapter exercises --

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Data.Semigroup

-- Semigroup and Monoid exercises

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

--1. Trivial

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial
    mappend = (<>)

instance Arbitrary Trivial where
    arbitrary = return Trivial

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

main1 :: IO ()
main1 = do
        quickCheck (semigroupAssoc :: TrivAssoc)
        quickCheck (monoidLeftIdentity :: Trivial -> Bool)
        quickCheck (monoidRightIdentity :: Trivial -> Bool)

-- 2. Identity

newtype Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen =  do 
                a <- arbitrary
                return (Identity a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

instance Semigroup a => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity $ x <> y

instance (Monoid a, Semigroup a) => Monoid (Identity a) where
    mappend = (<>)
    mempty = Identity mempty

type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

main2 :: IO ()
main2 = do
        quickCheck (semigroupAssoc :: IdentityAssoc)
        quickCheck (monoidLeftIdentity :: Identity String -> Bool)
        quickCheck (monoidRightIdentity :: Identity String -> Bool)

--3. Two

data Two a b = Two a b  deriving (Eq, Show)

twoGen :: (Arbitrary a, Arbitrary b) => Gen (Two a b)
twoGen =  do 
            a <- arbitrary            
            b <- arbitrary
            return (Two a b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = twoGen

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a1 b1) <> (Two a2 b2) = Two (a1 <> a2) (b1 <> b2)

instance (Semigroup a, Semigroup b, Monoid a, Monoid b) => Monoid (Two a b) where
    mappend = (<>)
    mempty = Two mempty mempty

type TwoAssoc = (Two String Ordering) -> (Two String Ordering) -> (Two String Ordering) -> Bool
type TwoIden = (Two String Ordering) -> Bool

main3 :: IO ()
main3 = do
        quickCheck (semigroupAssoc :: TwoAssoc)
        quickCheck (monoidLeftIdentity :: TwoIden)
        quickCheck (monoidRightIdentity :: TwoIden)

--4. Three

data Three a b c = Three a b c  deriving (Eq, Show)

threeGen :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (Three a b c)
threeGen =  do 
            a <- arbitrary            
            b <- arbitrary
            c <- arbitrary
            return (Three a b c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = threeGen

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a1 b1 c1) <> (Three a2 b2 c2) = Three (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance (Semigroup a, Semigroup b, Semigroup c, Monoid a, Monoid b, Monoid c) => 
    Monoid (Three a b c) where
        mappend = (<>)
        mempty = Three mempty mempty mempty
    
type ThreeAssoc = (Three String Ordering [Int]) -> 
        (Three String Ordering [Int]) -> 
        (Three String Ordering [Int]) -> Bool

type ThreeIden = (Three String Ordering [Int]) -> Bool

main4 :: IO ()
main4 = do
        quickCheck (semigroupAssoc :: ThreeAssoc)
        quickCheck (monoidLeftIdentity :: ThreeIden)
        quickCheck (monoidRightIdentity :: ThreeIden)

--5. Four

data Four a b c d = Four a b c d  deriving (Eq, Show)

fourGen :: (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Gen (Four a b c d)
fourGen =  do 
            a <- arbitrary            
            b <- arbitrary
            c <- arbitrary
            d <- arbitrary
            return (Four a b c d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = fourGen

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four a1 b1 c1 d1) <> (Four a2 b2 c2 d2) = Four (a1 <> a2) (b1 <> b2) (c1 <> c2) (d1 <> d2)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d, Monoid a, Monoid b, Monoid c, Monoid d) => 
    Monoid (Four a b c d) where
        mappend = (<>)
        mempty = Four mempty mempty mempty mempty

type FourAssoc = (Four String Ordering [Int] [Char]) -> 
        (Four String Ordering [Int] [Char]) -> 
        (Four String Ordering [Int] [Char]) -> Bool

type FourIden = (Four String Ordering [Int] [Char]) -> Bool 

main5 :: IO ()
main5 = do 
        quickCheck (semigroupAssoc :: FourAssoc)
        quickCheck (monoidLeftIdentity :: FourIden)
        quickCheck (monoidRightIdentity :: FourIden)

--6. BoolConj

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = BoolConj True 
    (BoolConj _) <> (BoolConj _) = BoolConj False

boolConjGen :: Gen BoolConj
boolConjGen = oneof [return $ BoolConj True, return $ BoolConj False]

instance Arbitrary BoolConj where
    arbitrary = boolConjGen

instance Monoid BoolConj where 
    mappend = (<>)
    mempty = BoolConj True

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool
type BoolConjIden = BoolConj -> Bool

main6 :: IO ()
main6 = do
        quickCheck (semigroupAssoc :: BoolConjAssoc)
        quickCheck (monoidLeftIdentity :: BoolConjIden)
        quickCheck (monoidRightIdentity :: BoolConjIden)

--7. BoolDisj

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance Semigroup BoolDisj where
    (BoolDisj True) <> _ = BoolDisj True 
    _ <> (BoolDisj True) = BoolDisj True
    _ <> _ = BoolDisj False

boolDisjGen :: Gen BoolDisj
boolDisjGen = oneof [return $ BoolDisj True, return $ BoolDisj False]

instance Arbitrary BoolDisj where
    arbitrary = boolDisjGen

instance Monoid BoolDisj where 
    mappend = (<>)
    mempty = BoolDisj False

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool
type BoolDisjIden = BoolDisj -> Bool

main7 :: IO ()
main7 = do
        quickCheck (semigroupAssoc :: BoolDisjAssoc)
        quickCheck (monoidLeftIdentity :: BoolDisjIden)
        quickCheck (monoidRightIdentity :: BoolDisjIden)

--8. or

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
    (Snd x) <> _ = Snd x
    (Fst x) <> s = s

orGen :: (Arbitrary a, Arbitrary b) => Gen (Or a b)
orGen = do
        a <- arbitrary
        b <- arbitrary
        oneof [return $ (Fst a), return $ (Snd b)]

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = orGen

type OrAssoc = Or Integer String -> Or Integer String -> Or Integer String -> Bool

main8 :: IO ()
main8 = quickCheck (semigroupAssoc :: OrAssoc)

--9. Combine
newtype Combine a b = Combine { unCombine :: (a -> b) } 

instance (Semigroup b) => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine (\a -> (f a) <> (g a))

threeCombineGen :: (Function a, CoArbitrary a, Arbitrary b) => (Gen (Combine a b, Combine a b, Combine a b))
threeCombineGen = do
             (Fun _ f) <- arbitrary
             (Fun _ g) <- arbitrary
             (Fun _ h) <- arbitrary
             return $ (Combine f, Combine g, Combine h)

combineGen :: (Function a, CoArbitrary a, Arbitrary b) => (Gen (Combine a b))
combineGen = do
             (Fun _ f) <- arbitrary
             return $ Combine f

instance (Function a, CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = combineGen

prop_assocFunc :: (Function a, CoArbitrary a, Arbitrary a, Arbitrary b, Semigroup b, Show a, Show b, EqProp a, EqProp b) => 
    (Gen (Combine a b, Combine a b, Combine a b)) -> Property
prop_assocFunc gen = forAll gen (\a b c -> (a <> (b <> c)) =-= ((a <> b) <> c))

instance (Show a, Show b, Arbitrary a, Arbitrary b, EqProp a, EqProp b) => EqProp (Combine a b) where
    (Combine f) =-= (Combine g) = f =-= g

instance Show (Combine a b) where
    show x = "Bool -> String" -- TODO: find a way to print real types

type CombineTripleType = ((Combine Bool String) , (Combine Bool String) , (Combine Bool String))

main9 :: IO ()
main9 = quickCheck (prop_assocFunc (threeCombineGen :: Gen CombineTripleType))

--10. Comp

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup (Comp a) where
    (Comp f) <> (Comp g) = Comp (f . g)

compGen :: (Function a, Arbitrary a, CoArbitrary a) => (Gen (Comp a))
compGen = do
             (Fun _ f) <- arbitrary
             return $ Comp f

threeCompGen :: (Function a, Arbitrary a, CoArbitrary a) => (Gen ((Comp a, Comp a, Comp a)))
threeCompGen = do
                (Fun _ f) <- arbitrary
                (Fun _ g) <- arbitrary
                (Fun _ h) <- arbitrary
                return $ (Comp f, Comp g, Comp h)

instance (Function a, Arbitrary a, CoArbitrary a) => Arbitrary (Comp a) where
    arbitrary = compGen

type CompAssoc a = (Comp a) -> (Comp a) -> (Comp a) -> Bool

prop_assocSimpleFunc :: (Function a, CoArbitrary a, Arbitrary a, Show a, EqProp a) => 
    (Gen (Comp a, Comp a, Comp a)) -> Property
prop_assocSimpleFunc gen = forAll gen (\a b c -> (a <> (b <> c)) =-= ((a <> b) <> c))

instance (Show a, Arbitrary a, EqProp a) => EqProp (Comp a) where
    (Comp f) =-= (Comp g) = f =-= g

instance Show (Comp a) where
    show x = "Comp a type" -- TODO: find a way to print real types

main10 :: IO ()
main10 = quickCheck (prop_assocSimpleFunc (threeCompGen :: Gen ((Comp Int, Comp Int, Comp Int))))

--11. Validation

data Validation a b = Failure' a | Success' b deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
    (Failure' x) <> (Failure' y) = Failure' (x <> y) 
    (Success' x) <> _ = Success' x
    _ <> (Success' y) = Success' y

main11 = do
    let failure :: String -> Validation String Int
        failure = Failure'
        success :: Int -> Validation String Int
        success = Success'
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2

-- Monoid exercises

--8.

newtype Mem s a = Mem { runMem :: s -> (a,s) }

instance (Semigroup a) => Semigroup (Mem s a) where 
    (Mem f) <> (Mem g) = Mem $ \x -> (fst (f x) <> fst (g x), snd $ g (snd (f x)))

instance Monoid a => Monoid (Mem s a) where
    mempty = Mem $ \x -> (mempty, x)
    mappend = (<>)

f' :: Mem Int String
f' = Mem $ \s -> ("hi", s + 1)

main12 = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0