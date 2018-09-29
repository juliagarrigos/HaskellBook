-- 8.3 Bottom --

f :: Bool -> Int
f True = error "blah"
f False = 0

-- Maybe
g :: Bool -> Maybe Int
g False = Just 0
g _ = Nothing