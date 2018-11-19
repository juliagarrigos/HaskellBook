-- 11.13 Constructing and deconstructing values --
data GuessWhat = Chickenbutt deriving (Eq, Show)
data Id a = MkId a deriving (Eq, Show)
data Product a b = Product a b deriving (Eq, Show)
data Sum a b = First a | Second b deriving (Eq, Show)
data RecordProduct a b = RecordProduct { pfirst :: a, psecond :: b } deriving (Eq, Show)

-- Sum and Product
newtype NumCow =
        NumCow Int
        deriving (Eq, Show)
newtype NumPig =
        NumPig Int
        deriving (Eq, Show)
data Farmhouse =
        Farmhouse NumCow NumPig
        deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
            NumSheep Int
            deriving (Eq, Show)
data BigFarmhouse =
            BigFarmhouse NumCow NumPig NumSheep
            deriving (Eq, Show)
type BigFarmhouse' =
            Product NumCow (Product NumPig NumSheep)

---

type Name = String
type Age = Int
type LovesMud = Bool

type PoundsOfWool = Int
data CowInfo = CowInfo Name Age deriving (Eq, Show)

data PigInfo =
    PigInfo Name Age LovesMud
    deriving (Eq, Show)
data SheepInfo = SheepInfo Name Age PoundsOfWool
    deriving (Eq, Show)

data Animal =
        Cow CowInfo
        | Pig PigInfo
        | Sheep SheepInfo
        deriving (Eq, Show)

-- Alternately
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

-- Constructing values

trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Integer
idInt = MkId 10

type Awesome = Bool
type Name' = String

person :: Product Name' Awesome
person = Product "Simon" True

data Twitter = Twitter deriving (Eq, Show)
data AskFm = AskFm deriving (Eq, Show)

socialNetwork :: Sum Twitter AskFm
socialNetwork = First Twitter


data OperatingSystem =
    GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill
    | Mac
    | Windows
    deriving (Eq, Show)
data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript
    | Kotlin
    deriving (Eq, Show)

data Programmer =
        Programmer { os :: OperatingSystem
        , lang :: ProgLang }
        deriving (Eq, Show)

-- We can reorder stuff when we use record syntax
feelingWizardly :: Programmer
feelingWizardly =
        Programmer { lang = Agda
        , os = GnuPlusLinux }

-- Exercise: Programmers
-- 1.

allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [ GnuPlusLinux
                        , OpenBSDPlusNevermindJustBSDStill
                        , Mac
                        , Windows
                        ]

allLanguages :: [ProgLang]
allLanguages = [Haskell, Agda, Idris, PureScript, Kotlin]

allProgrammers :: [Programmer]
allProgrammers = [Programmer x y | x <- allOperatingSystems, y <- allLanguages]

allProgrammers' :: [Programmer]
allProgrammers' = concat $ map (\x -> map (Programmer x) allLanguages) allOperatingSystems

-- Accidental bottoms from records

partialProgrammer :: Programmer
partialProgrammer = Programmer {os = Mac}

partialProgrammer':: ProgLang -> Programmer
partialProgrammer' = Programmer Mac

-- Deconstructing values

newtype Name'' = Name'' String deriving Show
newtype Acres = Acres Int deriving Show

-- FarmerType is a Sum
data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show

-- Farmer is a plain ole product of
-- Name, Acres, and FarmerType
data Farmer = Farmer Name'' Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec { name :: Name''
    , acres :: Acres
    , farmerType :: FarmerType }
    deriving Show
isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
    case farmerType farmer of
        DairyFarmer -> True
        _ -> False

-- Accidental bottoms from records
-- Never do this!
data Automobile = Null | Car { make :: String
                            , model :: String
                            , year :: Integer }
                            deriving (Eq, Show)

-- Don't do this either (now at least make' Null' don't compile)
data Car' = Car' { make' :: String
                , model' :: String
                , year' :: Integer }
                deriving (Eq, Show)


data Automobile' = Null' | Automobile' Car' deriving (Eq, Show)