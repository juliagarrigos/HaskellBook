-- Ciphers

import Text.Read (readMaybe)
import Data.Char (chr, ord)

-- Caesar Cipher
caesarRight :: String -> Int -> String
caesarRight [] _ = []
caesarRight (x:xs) s = shiftChar x s : caesarRight xs s

shiftChar :: Char -> Int -> Char
shiftChar ' ' _ = ' '
shiftChar c s = chr $ mod (ord c - ord 'a' + s ) 26 +  ord 'a'

unshiftChar :: Char -> Int -> Char
unshiftChar ' ' _ = ' '
unshiftChar c s = chr $ mod (ord c - ord 'a' - s ) 26 +  ord 'a'

uncaesarRight :: String -> Int -> String
uncaesarRight [] _ = []
uncaesarRight (x:xs) s = unshiftChar x s : uncaesarRight xs s

testingCaesar = uncaesarRight (caesarRight "helloworld" 7) 7 == "helloworld"

caesarEncode :: IO String
caesarEncode = do
  putStrLn "------ Caesar Cipher Encoder ------"
  putStr "Enter the message you want to encode: "
  message <- getLine
  if (length message)  == 0 
        then return "Error: the message should not be empty."
        else do
             putStr "Enter the shift number: "
             shift <- getLine
             case (readMaybe shift) :: Maybe Int of
                    Nothing ->  return "Error: shift should be a number"
                    Just n -> return ("The encoded message is: "++ caesarRight message n)
             

caesarDecode :: IO String
caesarDecode = do
    putStrLn "------ Caesar Cipher Decoder ------"
    putStr "Enter the message you want to decode: "
    message <- getLine
    if (length message)  == 0 
            then return "Error: the message should not be empty."
            else do
                 putStr "Enter the shift number: "
                 shift <- getLine
                 case (readMaybe shift) :: Maybe Int of
                        Nothing ->  return "Error: the shift should be a number"
                        Just n -> return ("The decoded message is: "++ uncaesarRight message n)

