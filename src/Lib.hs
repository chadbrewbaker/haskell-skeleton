module Lib
    ( someFunc, factorial
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)
