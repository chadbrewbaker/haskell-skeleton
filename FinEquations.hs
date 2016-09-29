interest ::(Num a) => a -> a -> a -> a 
interest p r t = p*r*t

netPresentValue' :: Float -> Float -> Float -> Float
netPresentValue' r i t = r /  ((1.0 + i ) ** t)

netPresentValue ::  [Float] -> Float -> Float -> Float
netPresentValue [] i t = 0.0
netPresentValue (x:xs) i t = netPresentValue' x i t + netPresentValue xs i (t+1.0)

netPresentValueArr ::  [Float] -> Float -> Float -> [Float]
netPresentValueArr [] i t = []
netPresentValueArr (x:xs) i t = netPresentValue' x i t : netPresentValueArr xs i (t+1.0)
            
monthsToZero :: Float -> Float -> Float  -> Int
monthsToZero principal rateMonthly payMonthly | principal <= 0  = 0
                                              | principal > 0 =  1 + monthsToZero ((principal - payMonthly)+ (principal - payMonthly)*rateMonthly) rateMonthly payMonthly

monthsOnSTDMortgage :: Float -> Int
monthsOnSTDMortgage = monthsToZero 200000 0.002871 

payments = take 9101 $ iterate (+1) (900 :: Float)

paybacks = map monthsOnSTDMortgage payments

pairs = zip payments paybacks

pairToCSV (a,b) = show a ++ "," ++ show b ++ "\n" 

csv = unlines $ map pairToCSV pairs

main = do
	    putStrLn "Payment,PaybackPeriod"
	    putStrLn csv

