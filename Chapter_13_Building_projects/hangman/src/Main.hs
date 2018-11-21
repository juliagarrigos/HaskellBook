module Main where

import Data.Char (toLower) 

import Puzzle
import GameWords

main :: IO ()
main = do
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle
