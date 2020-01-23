{- Assignment 1
 - Name: Frank Yang
 - Date: Sept 24, 2019
 -}
module Assign_1_ExtraCredit where
import Data.Complex
-- see https://www.stackage.org/haddock/lts-8.24/base-4.9.1.0/Data-Complex.html

macid = "yangf51"

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description: CubicQ takes a,b,c to generate the Q value
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = (3*a*c-b^2)/(9*a^2)
 
 {- -----------------------------------------------------------------
  - cubicR
  - -----------------------------------------------------------------
  - Description: CubicR takes a,b,c,d and generates the R value
  -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = (9*a*b*c-27*a^2*d-2*b^3)/(54*a^3)
 
 {- -----------------------------------------------------------------
  - cubicDisc
  - -----------------------------------------------------------------
  - Description: Determines the discriminant from q an r values calculated above
  -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = q^3 + r^2
 
 
 {- -----------------------------------------------------------------
  - cubeRt
  - -----------------------------------------------------------------
  - Description: Computes a cube root for one numbers
  -}
cubeRt :: Complex Double -> Complex Double 
cubeRt n = if realPart(n) < 0 then (-1)*((-1)*n)**(1/3) else n**(1/3)

 {- -----------------------------------------------------------------
  - cubicComplexS
  - -----------------------------------------------------------------
  - Description: Computes S value with complex float
  -}
cubicComplexS :: Double -> Double -> Complex Double
cubicComplexS q r = if cubicDisc q r < 0 then cubeRt(r :+ sqrt(abs(q^3+r^2))) else cubeRt((r + sqrt(q^3+r^2)) :+ 0) 

  {- -----------------------------------------------------------------
  - cubicComplexT
  - -----------------------------------------------------------------
  - Description: Computes T value with complex float
  -}
cubicComplexT :: Double -> Double -> Complex Double
cubicComplexT q r = if cubicDisc q r < 0 then cubeRt(r :+ (-1)*sqrt(abs(q^3+r^2))) else cubeRt((r-sqrt(q^3+r^2)) :+ 0) 

  {- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: Solve with complex solutions
 -}


cubicComplexSolutions :: Double -> Double ->  Double ->  Double -> [Complex Double]
cubicComplexSolutions a b c d = if disc < 0 then [] else if disc == 0 then [x1, x2, x3] else [x1, x2, x3] 
    where w = 0 :+ 1
          q = cubicQ a b c
          r = cubicR a b c d
          s = cubicComplexS q r
          t = cubicComplexT q r
          disc = cubicDisc q r
          x1 = (s+t-((b :+ 0)/((3*a) :+ 0)))
          x2 = -(s+t)/2 - ((b :+ 0)/((3*a) :+ 0)) + w*((sqrt(3)/2) * (s-t))
          x3 = -(s+t)/2 - ((b :+ 0)/((3*a) :+ 0)) - w*(sqrt(3)/2) * (s-t)