-- 9.3 Pattern matching on lists --
myHead :: [a] -> Maybe a
myHead [] = Nothing
myHead (x:_) = Just x

myTail :: [a] -> Maybe [a]
myTail [] = Nothing
myTail (_:[]) = Nothing
myTail (_:xs) = Just xs