-- 9.7 List comprehensions --
result = [ x^2 | x <- [1..10]]

resultWithCondition = [x^2 | x <- [1..10], rem x 2 == 0]

-- applies the function to all possible pairing of values from the two lists
multipleGenerators = [x^y | x <- [1..5], y <- [2, 3]]

multipleGeneratorsWC = [x^y | x <- [1..5], y <- [2, 3], x ^ y < 20]

createTuple = [(x,y) | x <- [1,2,3], y <- [6, 7]]

myList = [x^2 | x <- [1..10]]

usingMyList = [(x, y) | x <- myList, y <- [1..3], x < 4]

-- Exercises: Comprehend Thy Lists

-- mySqr :: [Integer]
-- mySqr = [x^2 | x <- [1..10]]

-- [x | x <- mySqr, rem x 2 == 0]
-- [(x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50]
-- take 5 [ (x, y) | x <- mySqr, y <- mySqr, x < 50, y > 50 ]

--Exercises: Square Cube
mySqr = [x^2 | x <- [1..5]] --[1,4,9,16,25]
myCube = [y^3 | y <- [1..5]] -- [1,8,27,64,125]

--1.
f :: [(Integer, Integer)]
f = [(x,y) | x <- mySqr, y <- myCube]

--2.
g :: [(Integer, Integer)]
g = [(x,y) | x <- mySqr, y <- myCube, x < 50, y < 50]