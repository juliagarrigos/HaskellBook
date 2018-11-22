
import Text.Read (readMaybe)

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid = NameEmpty | AgeTooLow | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
    putStrLn "------ Person constructor ------"
    putStr "Enter the name: "
    name <- getLine
    putStr "Enter the age: "
    age <- getLine
    case constructPerson name age of
            Right person -> putStrLn ("Yay! Successfully got a person: " ++ show person)
            Left NameEmpty -> putStrLn "Error: person name should not be empty."
            Left AgeTooLow -> putStrLn "Error: person age is too low."
            Left (PersonInvalidUnknown s) -> putStrLn ("Error: " ++ s)
    return()

constructPerson :: String -> String -> Either PersonInvalid Person
constructPerson name age = case (readMaybe age) :: Maybe Integer of
                                    Nothing -> Left (PersonInvalidUnknown "age should be a number")
                                    Just n -> mkPerson name n