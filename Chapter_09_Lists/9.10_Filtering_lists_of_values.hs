-- 9.10 Filtering lists of values --

-- Exercises: Filtering

--1.
--filter (\x -> rem x 3 == 0) [1..30]

--2.
--length . filter (\x -> rem x 3 == 0) $ [1..30]

--3.
myFilter :: String -> [String]
myFilter xs = filter (\x -> not . elem "a" $ ["the","a","an"]) (words xs)

testing = (myFilter "the brown dog was a goof") == ["brown","dog","was","goof"]