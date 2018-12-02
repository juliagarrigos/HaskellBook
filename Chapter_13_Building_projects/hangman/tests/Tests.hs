module Tests where

import Test.Hspec
import Test.QuickCheck
import Puzzle (Puzzle (Puzzle), fillInCharacter, handleGuess)

-- Puzzle for word engineer
puzzle :: Puzzle
puzzle = (Puzzle "engineer" [Just 'e', Nothing, Nothing, Just 'i', Nothing, Just 'e', Just 'e', Just 'r'] ['e','i','s','r'])

puzzleTest :: IO()
puzzleTest = hspec $ do
    describe "fillInCharacter" $ do
        it "when char appears once word should fills one empty space and add the char to the guessed list" $ (fillInCharacter puzzle 'g') 
            `shouldBe` (Puzzle "engineer" [Just 'e', Nothing, Just 'g' , Just 'i', Nothing, Just 'e', Just 'e', Just 'r'] ['g','e','i','s','r'])
        it "when char appears twice in word should fill two empty spaces and add the char to the guessed list" $ (fillInCharacter puzzle 'n') 
            `shouldBe` (Puzzle "engineer" [Just 'e', Just 'n', Nothing , Just 'i', Just 'n', Just 'e', Just 'e', Just 'r'] ['n','e','i','s','r'])
        it "when char is not in word should add it to the guessed list" $ (fillInCharacter puzzle 't') 
            `shouldBe` (Puzzle "engineer" [Just 'e', Nothing, Nothing, Just 'i', Nothing, Just 'e', Just 'e', Just 'r'] ['t','e','i','s','r'])
    describe "handleGuess" $ do
        it "when char was already guessed should not modify the puzzle" $ (handleGuess puzzle 'e') 
            `shouldReturn` (Puzzle "engineer" [Just 'e', Nothing, Nothing, Just 'i', Nothing, Just 'e', Just 'e', Just 'r'] ['e','i','s','r'])
        it "when char was not guessed and is in word should fill spaces and add the char to the guessed list" $ (handleGuess puzzle 'g') 
            `shouldReturn` (Puzzle "engineer" [Just 'e', Nothing, Just 'g' , Just 'i', Nothing, Just 'e', Just 'e', Just 'r'] ['g','e','i','s','r'])
        it "when char was not guessed and is not in word should add it to the guessed list" $ (handleGuess puzzle 't') 
            `shouldReturn` (Puzzle "engineer" [Just 'e', Nothing, Nothing, Just 'i', Nothing, Just 'e', Just 'e', Just 'r'] ['t','e','i','s','r'])