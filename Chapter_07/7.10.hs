-- Demonstrating composition

myPrint :: Show a => a -> IO ()
--myPrint x = putStrLn (show x)
--myPrint = \x -> putStrLn (show x)
--myPrint x = (putStrLn . show) x
myPrint = putStrLn . show

