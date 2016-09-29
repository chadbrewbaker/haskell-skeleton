module Main where

import Test.HUnit
import Test.QuickCheck
import Lib 

prop_factorial_monotone (NonNegative x) = factorial x <= (factorial x+1)

testZeroFactorial = TestCase $ assertEqual
  "Testing 0!"
  (1)
  (factorial 0)

testFiveFactorial = TestCase $ assertEqual 
  "Testing 5!"
  (1*2*3*4*5) 
  (factorial 5) 


main = do 
        someFunc
        runTestTT $ TestList[
          testFiveFactorial,
          testZeroFactorial]
        quickCheck prop_factorial_monotone


