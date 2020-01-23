{- Assignment 2
 - Name: Frank Yang
 - Date: Oct 16th 2019
 -}
module Assign_2 where

macid :: String
macid = "yangf51"

type GaussianInt = (Integer,Integer)

{- -----------------------------------------------------------------
 - gaussReal
 - -----------------------------------------------------------------
 - Description: Outputs the real component of the gaussianInt tuple
 -}
gaussReal :: GaussianInt -> Integer
gaussReal (x,y) = x

{- -----------------------------------------------------------------
 - gaussImag
 - -----------------------------------------------------------------
 - Description: Outputs the imaginary component of the gaussianInt tuple
 -}
gaussImag :: GaussianInt -> Integer
gaussImag (x,y) = y


{- -----------------------------------------------------------------
 - gausConj
 - -----------------------------------------------------------------
 - Description: Makes the imaginary component negative and outputs a gaussianNumber tuple
 -}
gaussConj :: GaussianInt -> GaussianInt
gaussConj g = (fst(g),-snd(g))

{- -----------------------------------------------------------------
 - gaussAdd
 - -----------------------------------------------------------------
 - Description: Adds two gaussian numbers by adding the real parts and imaginary parts together
 -}
gaussAdd :: GaussianInt -> GaussianInt -> GaussianInt
gaussAdd g0 g1 = (fst(g0)+fst(g1), snd(g0)+snd(g1))

{- -----------------------------------------------------------------
 - gaussMult
 - -----------------------------------------------------------------
 - Description: Multiplies two gaussian numbers through the formula a*b = a0b0-a1b1 + (a0b1+a1b0)i   where a = a0 + (a1)i
 -}
gaussMult :: GaussianInt -> GaussianInt -> GaussianInt
gaussMult g0 g1 = ((fst(g0)*fst(g1))-(snd(g0)*snd(g1)), (fst(g0)*snd(g1))+(snd(g0)*fst(g1)))

{- -----------------------------------------------------------------
 - gaussNorm
 - -----------------------------------------------------------------
 - Description: Outputs the normal of a gaussian integer, or the product of a gaussian number and it's conjugate
 -}
gaussNorm :: GaussianInt -> Integer
gaussNorm g = gaussReal (gaussMult g (gaussConj g) )

{- -----------------------------------------------------------------
 - maxGaussNorm
 - -----------------------------------------------------------------
 - Description: Takes a list and recursively calls itself to find the largest gaussian normal within the list. 
 - Uses the gaussian normal function from earlier along with a standard largest number in list implementation
 -}
maxGaussNorm :: [GaussianInt] -> GaussianInt
maxGaussNorm [] = (0,0)
maxGaussNorm [g] = g
maxGaussNorm (g:gs) = 
    if (gaussNorm(maxGaussNorm gs)) > gaussNorm(g) 
        then maxGaussNorm gs
        else g

{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 1
 - - Input: gaussConj (1,2)
 - - Expected Output: (1,-2)
 - - Acutal Output: (1,-2)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 2 
 - - Input: gaussConj (-1,-2)
 - - Expected Output: (-1,2)
 - - Acutal Output: (-1,2)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 3
 - - Input: gaussConj (3,-3)
 - - Expected Output: (3,3)
 - - Acutal Output: (3,3)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 1
 - - Input: gaussAdd (3,-3) (2,2)
 - - Expected Output: (5,-1)
 - - Acutal Output: (5,-1)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 2
 - - Input: gaussAdd  (1,1) (-2,-2)
 - - Expected Output: (-1,-1)
 - - Acutal Output: (-1,-1)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 3
 - - Input: gaussAdd (3,-3) (-3, 3)
 - - Expected Output: (0,0)
 - - Acutal Output: (0,0)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 1
 - - Input: gaussMult (1,2) (2,3)
 - - Expected Output:  (-4,7)
 - - Acutal Output:  (-4,7)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 2
 - - Input: gaussMult (-2, -2) (2, 2)
 - - Expected Output: (0,-8)
 - - Acutal Output: (0,-8)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 3
 - - Input: gaussMult (-2, 1) (1, -2)
 - - Expected Output: (0,5)
 - - Acutal Output: (0,5)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 1
 - - Input: gaussNorm (2, 3)
 - - Expected Output: 13
 - - Acutal Output: 13
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 2
 - - Input: gaussNorm (-2, 1) 
 - - Expected Output: 5
 - - Acutal Output: 5
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 3
 - - Input: gaussNorm (1, -2)
 - - Expected Output: 5
 - - Acutal Output: 5
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 1
 - - Input: maxGaussNorm [(1, 2), (2,3), (-3,-4)]
 - - Expected Output: (-3,-4)
 - - Acutal Output: (-3,-4)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 2
 - - Input: maxGaussNorm []
 - - Expected Output: (0,0)
 - - Acutal Output: (0,0)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 3
 - - Input: maxGaussNorm [(-1, -2)]
 - - Expected Output: (-1,-2)
 - - Acutal Output: (-1,-2)
 - -----------------------------------------------------------------
 -
 -
 -}

