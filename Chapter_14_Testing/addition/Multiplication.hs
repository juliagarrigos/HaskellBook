module Multiplication where

import Test.Hspec

-- myMult 2 3 = 2 + 2 + 2 = 6
myMult :: Integral a => a -> a -> a
myMult x y = go 0 0 
    where go n c 
            | c == y = n
            | otherwise = go (n+x) (c+1)

main :: IO ()
main = hspec $ do
        describe "Multiplication" $ do
            it "2 multiplied by 5 is 10" $ do
                myMult 2 5 `shouldBe` 10
            it "2 multiplied by 3 is 6" $ do
                myMult 2 3 `shouldBe` 6