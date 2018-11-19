-- 9.4 List’s syntactic sugar --

-- No syntactic sugar
myList = (1 : 2 : 3 : []) ++ 4 : []

-- Syntactic sugar
myListSs = [1,2,3] ++ [4]