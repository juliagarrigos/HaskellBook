-- 11.11 Product types --

data QuantumBool = QuantumTrue | QuantumFalse | QuantumBoth deriving (Eq, Show)

data TwoQs = MkTwoQs QuantumBool QuantumBool deriving (Eq, Show)

type TwoQs' = (QuantumBool, QuantumBool)

--Record syntax
data Person = MkPerson String Int deriving (Eq, Show)

jm = MkPerson "julie" 18
ca = MkPerson "chris" 16

namae :: Person -> String
namae (MkPerson s _) = s


data Person' = Person' { name :: String, age :: Int } deriving (Eq, Show)

-- You can't create records with same fields
--data Cat = Cat { name :: String, age :: Int } deriving (Eq, Show) 

p1 = Person' "julie" 18
p2 = Person' "chris" 16

age1 = age p1
name1 = name p1