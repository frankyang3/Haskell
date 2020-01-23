{- Assignment 5 Extra Credit
 - Name: Frank Yang
 - Date: Nov 27 2019
 -}
module Assign_5_ExtraCredit where

import System.Random
import Control.Monad

macid :: String
macid = "yangf51"

--Generates a uniform tuple between 0 and 1 We use these as coordinates
uniformSample2D :: (Random a, Floating a) => IO(a,a)
uniformSample2D = do a <- randomRIO(0,1)
                     b <- randomRIO(0,1)
                     return (a,b)

--This function applies the box mller transform to two points
uniformToNorm :: Floating a => a -> a -> (a,a)
uniformToNorm x y = (sqrt((-2)*log(x)) * cos(2*pi*y),sqrt((-2)*log(x)) * sin(2*pi*y))

--This generates a tuple using uniformToNorm, or Box-Muller transoforms
normalSample2D :: (Random a, Floating a) => IO (a,a)
normalSample2D = do 
                    x <- randomRIO(0,1)
                    y <- randomRIO(0,1)
                    return (uniformToNorm x y)

--Special concat function used for getSamples underneath
conc :: (Random a, Floating a) => IO (a,a) -> IO [(a,a)] -> IO [(a,a)]
conc a b = do x <- a
              y <- b
              return (x : y)

--This generates a list of samples, or a list of tuples based on a generator function
--It uses a special concat function to do so
genSamples :: (Random a, Floating a) => IO (a,a) -> Int -> IO [(a,a)]
genSamples gen 1 = do a <- gen
                      return [a]
genSamples gen n = do conc gen (genSamples gen (n-1))

--A hit checker to see if a point is inside the unit circle
--If x^2 + y^2 <= then 1
--else 2
unitCircleHit :: (Floating a,Ord a) => (a,a) -> Bool   
unitCircleHit a= if (fst(a)**2+snd(a)**2 <= 1) then True else False

--This adds everything inside the sigma in monte carlo integration, it starts by generating a list of points
--If the point lies inside the hit formula, then it will add 1, otherwise it will add 0. 
--This continues until all the points in the list have been checked and accounted for
monteCarlo2Dsmall :: Floating a => a -> a -> [(a,a)] -> ((a,a) -> Bool) -> a
monteCarlo2Dsmall max may [] h = 0
monteCarlo2Dsmall max may (l:ls) h = if (h l) == True then (max * may) + (monteCarlo2Dsmall max may ls h)  else 0 + (monteCarlo2Dsmall max may ls h)


--This uses Monte Carlo integration. it simply divides everything inside the sigma notation by the total length of the list (1/n)
--This gives us an estimate the of the area under the curve
monteCarlo2D :: Floating a => a -> a -> [(a,a)] -> ((a,a) -> Bool) -> a
monteCarlo2D max may ls h =  (monteCarlo2Dsmall max may ls h) /fromIntegral(length ls) 


--This uses monte carlo integration to estimate the area of a quarter unit circle (quadrant 1). 
--We then multiply this by 4 to obtain the full area of the unit circle, which should be equal to pi
--uniform sampling works the best in terms of estimation
estimatePi :: IO (Double,Double) -> Int -> IO Double

estimatePi samp n = do x <- genSamples uniformSample2D 100
                       return (4 * monteCarlo2D 1 1 x unitCircleHit)
                  


{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: uniformSample2D
 - - Test Case Number: 1
 - - Input: uniformSample2D
 - - Expected Output: A tuple
 - - Acutal Output: (2.1230473705097452e-2,0.668053460985396)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: uniformSample2D
 - - Test Case Number: 2
 - - Input: uniformSample2D
 - - Expected Output: A tuple
 - - Acutal Output: (0.9798518379027575,0.3818754986117018)
 - -----------------------------------------------------------------
  - -----------------------------------------------------------------
 - - Function: uniformSample2D
 - - Test Case Number: 3
 - - Input: uniformSample2D
 - - Expected Output: A tuple
 - - Acutal Output: (0.5483098969506092,0.9001101622056066)
 - -----------------------------------------------------------------
   - -----------------------------------------------------------------
 - - Function: uniformToNorm 
 - - Test Case Number: 1
 - - Input: uniformToNorm 1 1
 - - Expected Output: (-0.0,0.0)
 - - Acutal Output: (-0.0,0.0)
 - -----------------------------------------------------------------
   - -----------------------------------------------------------------
 - - Function: uniformToNorm 
 - - Test Case Number: 2
 - - Input: uniformToNorm 0.5 0.5
 - - Expected Output: (-1.1774100225154747,1.4419114153575892e-16)
 - - Acutal Output: (-1.1774100225154747,1.4419114153575892e-16)
 - -----------------------------------------------------------------
   - -----------------------------------------------------------------
 - - Function: uniformToNorm 
 - - Test Case Number: 3
 - - Input: funiformToNorm 0.2 0.5
 - - Expected Output: (-1.7941225779941015,2.197166472418474e-16)
 - - Acutal Output: (-1.7941225779941015,2.197166472418474e-16)
 - -----------------------------------------------------------------
 -- QUICKCHECK
 -- Function: uniformToNorm
 -- Property: propUniformToNorm
 -- Result: PASS
    - -----------------------------------------------------------------
 - - Function: normalSample2D
 - - Test Case Number: 1
 - - Input: normalSample2D
 - - Expected Output: A tuple
 - - Acutal Output: (0.9212096146570113,-0.7803415941353945)
 - -----------------------------------------------------------------
    - -----------------------------------------------------------------
 - - Function: normalSample2D
 - - Test Case Number: 2
 - - Input: normalSample2D
 - - Expected Output: A tuple
 - - Acutal Output: (0.586978366285757,-4.330324948072097e-2)
 - -----------------------------------------------------------------
    - -----------------------------------------------------------------
 - - Function: normalSample2D
 - - Test Case Number: 3
 - - Input: normalSample2D
 - - Expected Output: A tuple
 - - Acutal Output: (-8.000937247164862e-2,0.45531805020560084)
 - -----------------------------------------------------------------
  - -----------------------------------------------------------------
 - - Function: genSamples
 - - Test Case Number: 1
 - - Input: genSamples uniformSample2D 10
 - - Expected Output: [(0.3930669039116028,0.9873314629765205),(0.7321366682140131,0.2850285412665665),(0.8632996732150966,0.950110493106122),(0.9312393066737887,0.7716965821696526),(0.23711603043528218,0.3689425495194608),(5.5319487475696705e-2,0.6541681718402765),(0.22916426462784278,0.11590657956852413),(0.9383751989224287,0.8943951758347116),(0.3500304127522441,0.9808603010574973),(0.992569559151139,0.5075741454598643)]
 - - Acutal Output: [(0.3930669039116028,0.9873314629765205),(0.7321366682140131,0.2850285412665665),(0.8632996732150966,0.950110493106122),(0.9312393066737887,0.7716965821696526),(0.23711603043528218,0.3689425495194608),(5.5319487475696705e-2,0.6541681718402765),(0.22916426462784278,0.11590657956852413),(0.9383751989224287,0.8943951758347116),(0.3500304127522441,0.9808603010574973),(0.992569559151139,0.5075741454598643)]
 - -----------------------------------------------------------------
    - -----------------------------------------------------------------
 - - Function: genSamples
 - - Test Case Number: 2
 - - Input: genSamples uniformSample2D 1
 - - Expected Output: [(0.7711303958455314,0.9714198229407008)]
 - - Acutal Output: [(0.7711303958455314,0.9714198229407008)]
 - -----------------------------------------------------------------
    - -----------------------------------------------------------------
 - - Function: genSamples
 - - Test Case Number: 3
 - - Input: genSamples uniformSample2D 2
 - - Expected Output: [(0.6552641096907151,0.4324537009930858),(0.21057971003015208,0.8634065195020472)]
 - - Acutal Output: [(0.6552641096907151,0.4324537009930858),(0.21057971003015208,0.8634065195020472)]
 - -----------------------------------------------------------------
  - -----------------------------------------------------------------
 - - Function: unitCircleHit
 - - Test Case Number: 1
 - - Input: unitCircleHit(0.1,0.1)
 - - Expected Output: True
 - - Acutal Output: True
 - -----------------------------------------------------------------
    - -----------------------------------------------------------------
 - - Function: unitCircleHit
 - - Test Case Number: 2
 - - Input: unitCircleHit(0.5,0.1)
 - - Expected Output: True
 - - Acutal Output: True
 - -----------------------------------------------------------------
    - -----------------------------------------------------------------
 - - Function: unitCircleHit
 - - Test Case Number: 3
 - - Input: unitCircleHit(7,7)
 - - Expected Output: False
 - - Acutal Output: False
 - -----------------------------------------------------------------
 -- QUICKCHECK
 -- Function: unitCircleHit
 -- Property: propUnitCircleHit
 -- Result: PASS
 -}

















