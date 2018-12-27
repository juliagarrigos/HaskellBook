module Ciphers where

import Data.Char(chr, ord)

-- Ciphers

-- Caesar Cipher
caesarRight :: String -> Int -> String
caesarRight [] _ = []
caesarRight (x:xs) s = shiftChar x s : caesarRight xs s

shiftChar :: Char -> Int -> Char
shiftChar c s = chr $ mod (ord c - ord 'a' + s ) 26 +  ord 'a'

unshiftChar :: Char -> Int -> Char
unshiftChar c s = chr $ mod (ord c - ord 'a' - s ) 26 +  ord 'a'

uncaesarRight :: String -> Int -> String
uncaesarRight [] _ = []
uncaesarRight (x:xs) s = unshiftChar x s : uncaesarRight xs s

-- Vigenère cipher

alphabetIndex :: Char -> Int
alphabetIndex x = ord x - ord 'a'

encode :: (Char, Char) -> Char
encode (x, y) = shiftChar x (alphabetIndex y)

decode :: (Char, Char) -> Char
decode (x, y) = unshiftChar x (alphabetIndex y)

vigenere :: String -> String -> String
vigenere message key = map encode $ zipWith (,) message (concat $ repeat key)

unvigenere :: String -> String -> String
unvigenere message key = map decode $ zipWith (,) message (concat $ repeat key)