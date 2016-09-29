
module Main where

import Criterion.Main
import Lib 

main = defaultMain [
  bgroup "factorial" [ bench "2" $ whnf factorial 2
  					 , bench "16" $ whnf factorial 16
  					 , bench "32" $ whnf factorial 32
  					 , bench "200" $ whnf factorial 200
  					 , bench "5000" $ whnf factorial 5000
  					 ]
  ]
