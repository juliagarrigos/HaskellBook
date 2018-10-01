-- 9.6 Extracting portions of lists --

-- Exercises: Thy Fearful Symmetry

-- 1.
myWords :: [Char] -> [[Char]]
myWords [] = []
myWords a = go a []
    where   word = takeWhile (/=' ')
            rest = dropWhile (==' ') . dropWhile (/=' ')
            go :: [Char] -> [[Char]] -> [[Char]]
            go t ws 
                | elem ' ' t = go (rest t) (ws ++ [word t])
                | otherwise = (ws ++ [word t])

--2.
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines a = go a []
        where   sentence = takeWhile (/= '\n')
                rest = dropWhile (== '\n') . dropWhile (/= '\n')
                go :: String -> [String] -> [String]
                go t ss 
                        | elem '\n' t = go (rest t) (ss ++ [sentence t])
                        | otherwise = ss ++ [t]

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"]

    -- Test
main :: IO ()
main = print $ "Are they equal? "++ show (myLines' sentences == shouldEqual)

--3.
mySplit :: String -> Char -> [String]
mySplit [] _ = []
mySplit a b = go a []
        where   part = takeWhile (/=b)
                rest = dropWhile (== b) . dropWhile (/= b)
                go :: String -> [String] -> [String]
                go t ss 
                        | elem b t = go (rest t) (ss ++ [part t])
                        | otherwise = ss ++ [t]


myWords' :: [Char] -> [[Char]]
myWords' a = mySplit a ' '

myLines' :: String -> [String]
myLines' a = mySplit a '\n'
