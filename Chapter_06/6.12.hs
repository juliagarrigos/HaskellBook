module Example where

-- This is only to make a point. Please do not write typeclasses like this and never use typeclasses to define default values.

class Numberish a where
    fromNumber :: Integer -> a
    toNumber :: a -> Integer
    defaultNumber :: a

-- pretend newtype is data for now
newtype Age = Age Integer deriving (Eq, Show)

instance Numberish Age where
    fromNumber n = Age n
    toNumber (Age n) = n
    defaultNumber = Age 65

newtype Year = Year Integer deriving (Eq, Show)

instance Numberish Year where
    fromNumber n = Year n
    toNumber (Year n) = n
    defaultNumber = Year 1988

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
    where integerOfA = toNumber a
          integerOfAPrime = toNumber a'
          summed = integerOfA + integerOfAPrime