module Main where

import Test.QuickCheck
import Skeleton

prop_factorial_monotone (Positive x) = factorial x <= (factorial x+1)

main =	quickCheck prop_factorial_monotone 
