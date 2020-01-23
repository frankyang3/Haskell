{- Assignment 1
 - Name: Frank Yang
 - Date: Sept 22, 2019
 -}
module Assign_1 where

macid :: String
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
cubeRt :: Double -> Double
cubeRt n = if n < 0 then (-1)*((-1)*n)**(1/3) else n**(1/3)

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description: CubicS uses q and r values to calculate S
 -}
cubicS :: Double -> Double -> Double
cubicS q r = cubeRt(r + sqrt(q^3+r^2))

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description: cubicT takes r and q to generate the T value
 -}
cubicT :: Double -> Double -> Double
cubicT q r = cubeRt(r-sqrt(q^3+r^2))

{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: Solve with real solutions
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d = if disc < 0 then [] else if disc == 0 then [x1,x2,x3] else [x1] 
    where q = cubicQ a b c
          r = cubicR a b c d
          s = cubicS q r
          t = cubicT q r
          disc = cubicDisc q r
          x1 = s+t-(b/(3*a))
          x2 = -(s+t)/2 - b/(3*a) 
          x3 = -(s+t)/2 - b/(3 *a) 


{- -----------------------------------------------------------------
 - Test Cases 
 - -----------------------------------------------------------------
 -}

{-- 
cubicRealSolutions 1 0 (-3) 0   gives  []
cubicRealSolutions 1 2 3 4      gives  [-1.651]
cubicQ 1 2 3                    gives 0.5555555
cubicR 1 2 3 4                  gives -1.296296
cubicDisc 0.5555 (-1.296296)    gives 1.851799
cubicS 0.5555 (-1.296296)       gives 0.40074
cubicT 0.5555 (-1.296296)       gives -1.38517


-}

