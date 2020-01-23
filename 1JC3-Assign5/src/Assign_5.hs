{- Assignment 5
 - Name: Frank Yang
 - Date: Nov 21 2019
 -}
module Assign_5 where

macid :: String
macid = "yangf51"


{- -----------------------------------------------------------------
 - definiteIntegral
 - -----------------------------------------------------------------
 - Description: Utilizes functions calcDx and trapIntegral
 - trapIntegral calculates the integral given a constant dx, it recursively adds the areas of each trapezoid of width dx until the number of trapezoids are reached
 - It terminates when the base case is reached, n = 0, aka when the designated number trapezoids have been added. Note that a <= b in order for the program to work
 - calcDx calculates dx based on (b-a)/n, this is used in order to generate a constant dx
 - definiteIntegral combines the above 2 functions by simply inputing the dx in trapIntegral as the result of calcDx while error checking bounds
 -}

trapIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
trapIntegral _ _ _ 0 = 0
trapIntegral dx a g n =  dx * ((g a) + (g (a+dx)))/2 + (trapIntegral dx (a+dx) g (n-1)) 

calcDx :: Double -> Double -> Integer -> Double
calcDx a b n = (b-a)/(fromInteger n)

definiteIntegral :: Double -> Double -> (Double -> Double) -> Integer -> Double
definiteIntegral a b g n = if(b>=a)  then trapIntegral (calcDx a b n) a g n else (error "bounds do not make sense")

{- -----------------------------------------------------------------
 - funH
 - -----------------------------------------------------------------
 - Description: Uses the funH functiona, x**(1/n) and x**n. Using this definition we can find the area between the curves through subtraction
 - The new integral is found to be x**(1/n)-x**(n). We integrate over this integral using the above function. If n is negative, 
 - the function is not in range and is thus not defined
 -}
funH :: Integer -> Double
funH n = if(n>0) then definiteIntegral 0 1 (\x -> x**(1/fromInteger n)-x**(fromInteger n)) 10000
        else error ("not in range")

{- -----------------------------------------------------------------
 - funK
 - -----------------------------------------------------------------
 - Description: Similar to funH, we find the integral between -1 and 1 of the function n**x If n is negative, the function is assumed
 - to be undefined and is thus not in range.
 -}
funK :: Double -> Double
funK n = if(n > 0) then definiteIntegral (-1) 1 (\x -> n**x) 10000
        else error ("not in range")

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 1
 - - Input: definiteIntegral 0 1 (\x -> x) 100
 - - Expected Output: 0.5
 - - Acutal Output: 0.5
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 2
 - - Input: definiteIntegral 0 1 (\x -> x**2) 1
 - - Expected Output: 0.5
 - - Acutal Output: 0.5
 - -----------------------------------------------------------------
  - -----------------------------------------------------------------
 - - Function: definiteIntegral
 - - Test Case Number: 3
 - - Input: definiteIntegral 0 1 (\x -> x**2) 1000000
 - - Expected Output: 0.3333333333340053
 - - Acutal Output: 0.3333333333340053
 - -----------------------------------------------------------------
 -- QUICKCHECK
 -- Function: definiteIntegral
 -- Property: propDefiniteIntegral
 -- Result: PASS
   - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 1
 - - Input: funH 10
 - - Expected Output: 0.818165199784418
 - - Acutal Output: 0.818165199784418
 - -----------------------------------------------------------------
   - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 2
 - - Input: funH 100
 - - Expected Output: 0.980153164958415
 - - Acutal Output: 0.980153164958415
 - -----------------------------------------------------------------
   - -----------------------------------------------------------------
 - - Function: funH
 - - Test Case Number: 3
 - - Input: funH 1
 - - Expected Output: 0.0
 - - Acutal Output: 0.0
 - -----------------------------------------------------------------
 -- QUICKCHECK
 -- Function: funH
 -- Property: propFunH
 -- Result: PASS
    - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 1
 - - Input: funK 1
 - - Expected Output: 1.9999999999998124
 - - Acutal Output: 1.9999999999998124
 - -----------------------------------------------------------------
    - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 2
 - - Input: funK 10
 - - Expected Output: 4.299515446826352
 - - Acutal Output: 4.299515446826352
 - -----------------------------------------------------------------
    - -----------------------------------------------------------------
 - - Function: funK
 - - Test Case Number: 3
 - - Input: funK 1000
 - - Expected Output: 144.76470556194525
 - - Acutal Output: 144.76470556194525
 - -----------------------------------------------------------------
 -- QUICKCHECK
 -- Function: funK
 -- Property: propFunK
 -- Result: PASS
 -}

