module Example where

isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

isItTwo' :: Integer -> Bool
isItTwo' 2 = True

--RegisteredUser

newtype Username = Username String
newtype AccountNumber = AccountNumber Integer

data User = UnregisteredUser| RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser(Username name)(AccountNumber acctNum)) =
    putStrLn $ name ++ " " ++ show acctNum

---

data WherePenguinsLive = Galapagos | Antarctica | Australia | SouthAfrica | SouthAmerica
    deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _ = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p = (galapagosPenguin p) || (antarcticPenguin p)

-- TupleFunctions

addEmUp2 :: Num a => (a, a) -> a
addEmUp2 (x, y) = x + y

addEmUp2Alt :: Num a => (a, a) -> a
addEmUp2Alt tup = (fst tup) + (snd tup)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, x) = x

-- Exercises: Variety Pack

-- 1.
k :: (a, b) -> a
k (x, y) = x

k1 :: Num a => a
k1 = k ((4-1), 10) -- 3

k2 :: [Char]
k2 = k ("three", (1 + 2)) -- "three"

k3 :: Num a => a
k3 = k (3, True) -- 3

-- 2.
g :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
g (a, b, c) (d, e, f) =  ((a, d), (c, f))