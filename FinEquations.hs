interest ::(Num a) => a -> a -> a -> a 
interest p r t = p*r*t

netPresentValue' :: Float -> Float -> Float -> Float
netPresentValue' r i t = r /  ((1.0 + i ) ** t)


-- each year I take a d=$5000 loan to make a contribution
-- roth earnings compounded at r1
-- roth loan compounded at r2

rothStep (rothbal, loanbal, contribs,intpaid) contrib r1 r2 = 
    ( (rothbal + (rothbal * r1) + contrib), (loanbal  + contrib), (contribs + contrib), (intpaid + loanbal * r2) )



describe (rothbal,  loanbal, contribs, intpaid) = "TaxfreeIncome: " ++ show rothbal ++ "\nPayable can caploss against capgains: " ++show loanbal ++ "\nContribBasis: " ++show contribs ++ "\nInterest deducted from taxed earnings: "++show intpaid
 --     where
   --   loss = loanbal - contribs
     -- earnings = rothbal - contribs

rothSteps years (rothbal, loanbal, contribs, intpaid) contrib r1 r2 = if years == 0 then 
                                                                (rbalnext, loanbalnext, contribnext, intpaidnext) 
                                                                else
                                                                rothSteps (years-1) (rbalnext, loanbalnext, contribnext, intpaidnext) contrib r1 r2
                where
                (rbalnext, loanbalnext, contribnext, intpaidnext) = rothStep (rothbal, loanbal, contribs, intpaid) contrib r1 r2 


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
        --putStrLn "Payment,PaybackPeriod"
        --putStrLn csv
        putStrLn $ describe $ rothSteps 20 (0.0, 0.0,0.0,0.0) 5000.0 0.05 0.05

