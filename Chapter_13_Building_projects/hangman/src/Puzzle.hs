module Puzzle where

import Data.List (intersperse) 
import System.Exit (exitSuccess) 
import Data.Maybe (isJust) 
import Control.Monad (forever) 

maxIncorrectAttempts :: Int
maxIncorrectAttempts = 7

data Puzzle = Puzzle String [Maybe Char] [Char] 
instance Show Puzzle where
    show (Puzzle _ discovered guessed) = (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (\x -> Nothing) word) [] 

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) char = elem char word 

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = elem char guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
        where zipper :: Char -> Char -> (Maybe Char) -> (Maybe Char)
              zipper guessed wordChar guessChar =
                        if wordChar == guessed
                        then Just wordChar
                        else guessChar 
              newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that\
                \ character, pick \
                \ something else!"
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the\
                    \ word, filling in the word\
                    \ accordingly"
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "This character wasn't in\
                    \ the word, try again."
            return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle wordToGuess _ guessed) =
    if (guessedIncorrect puzzle) > maxIncorrectAttempts then
        do  putStrLn "You lose!"
            putStrLn $ "The word was: " ++ wordToGuess
            exitSuccess
    else return ()

guessedIncorrect :: Puzzle -> Int
guessedIncorrect (Puzzle word _ guessed) = length $ filter (\x -> not $ elem x word) guessed

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    if all isJust filledInSoFar then
        do  putStrLn "You win!"
            exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "Your guess must\
            \ be a single character"