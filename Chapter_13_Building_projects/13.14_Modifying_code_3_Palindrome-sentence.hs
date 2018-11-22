import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower)

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case (isPalindrome line1) of
            True -> putStrLn "It's a palindrome!"
            False -> do
                     putStrLn "Nope!"
                     exitSuccess 

isPalindrome :: String -> Bool
isPalindrome text = f text == reverse (f text)
    where f l = filter (\x -> elem x ['a'..'z']) (map toLower l)

testIsPalindrome = isPalindrome "Madam I'm Adam" == True