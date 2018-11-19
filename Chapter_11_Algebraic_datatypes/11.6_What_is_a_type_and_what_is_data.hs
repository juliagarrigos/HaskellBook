-- 11.6 What’s a type and what’s data? --

data Price = Price Integer deriving (Eq, Show)

data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)

data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)

data Vehicle = Car Manufacturer Price | Plane Airline deriving (Eq, Show)

-- Exercises: Vehicles

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

--2.

isCar :: Vehicle -> Bool
isCar (Car _ _ ) = True
isCar _ = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ ) = True
isPlane _ = False

areCars :: [Vehicle] -> [Bool]
areCars vs = map isCar vs

--3.
getManu :: Vehicle -> Manufacturer
getManu (Car manu _) = manu
--getManu _ = error "No manufacturer for planes"

--4. Non-exhaustive patterns in function getManu

--5.
type Size = Integer

data Vehicle' = Car' Manufacturer Price | Plane' Airline Size deriving (Eq, Show)