{-# LANGUAGE FlexibleContexts,FlexibleInstances,IncoherentInstances #-}
{- Assignment 2 Extra Credit
 - Name: Frank Yang
 - Date: Oct 17th 2019
 -}
module Assign_2_ExtraCredit where

import Data.Complex

macid = "yangf51"


data GaussianInt a = a :@ a
  deriving (Show)

  --GaussianIntegral Class
class GaussianIntegral g where
  gaussZero :: Integral a => g a
  gaussReal :: Integral a => g a -> a
  gaussImag :: Integral a => g a -> a
  gaussConj :: Integral a => g a -> g a
  gaussAdd :: Integral a => g a -> g a -> g a
  gaussMult :: Integral a => g a -> g a -> g a

--Defines instance for type GaussianInt
instance GaussianIntegral GaussianInt where
  gaussZero = 0 :@ 0
  gaussReal (x :@ y) = x 
  gaussImag (x :@ y) = y
  gaussConj (x :@ y) = (x :@ (-y))
  gaussAdd g0 g1 = (gaussReal g0 + gaussReal g1) :@ (gaussImag g0 + gaussImag g1)
  gaussMult g0 g1 = ((gaussReal(g0)*gaussReal(g1))-(gaussImag(g0)*gaussImag(g1))) :@ ((gaussReal(g0)*gaussImag(g1))+(gaussImag(g0)*gaussReal(g1)))
 
--Defines instance for type Complex (from Data.Complex)
instance GaussianIntegral Complex where
  gaussZero = 0 :+ 0
  gaussReal (x :+ y) = x
  gaussImag (x :+ y) = y
  gaussConj (x :+ y) = (x :+ (-y))
  gaussAdd g0 g1 = (realPart g0 + realPart g1) :+ (imagPart g0 + imagPart g1)
  gaussMult g0 g1 = ((realPart(g0)*realPart(g1))-(imagPart(g0)*imagPart(g1))) :+ ((realPart(g0)*imagPart(g1))+(imagPart(g0)*realPart(g1)))

--implements new definitions for Eq and Ord for GaussianIntegral
instance (Integral a, GaussianIntegral g) => Eq(g a) where
  x == y = (gaussNorm x) == (gaussNorm y)

instance (Integral a, GaussianIntegral g) => Ord(g a)  where
  compare x y = compare (gaussNorm x) (gaussNorm y)

--Implementation of gaussNorm and maxGaussNorm following assign_2 and the extra credit assignment sheet
gaussNorm :: (Integral a, GaussianIntegral g) => g a -> a
gaussNorm g = gaussReal (gaussMult g (gaussConj g) )

maxGaussNorm :: (Integral a, GaussianIntegral g) => [g a] -> g a
maxGaussNorm gs = foldr max gaussZero gs


{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 1
 - - Input: gaussConj 1 :@ 2
 - - Expected Output: 1 :@ (-2)
 - - Acutal Output: 1 :@ (-2)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 2 
 - - Input: gaussConj (-1) :+ (-2)
 - - Expected Output: (-1) :+ 2
 - - Acutal Output: (-1) :+ 2
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussConj
 - - Test Case Number: 3
 - - Input: gaussConj 3 :@ (-3)
 - - Expected Output: 3 :@ 3
 - - Acutal Output: 3 :@ 3
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 1
 - - Input: gaussAdd (3 :+ (-3)) (2 :+ 2)
 - - Expected Output: 5 :+ (-1)
 - - Acutal Output: 5 :+ (-1)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 2
 - - Input: gaussAdd  (1 :@ 1) ((-2) :@ (-2))
 - - Expected Output: (-1) :@ (-1)
 - - Acutal Output: (-1) :@ (-1)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussAdd
 - - Test Case Number: 3
 - - Input: gaussAdd (3 :+ (-3)) ((-3) :+ 3)
 - - Expected Output: 0 :+ 0
 - - Acutal Output: 0 :+ 0
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 1
 - - Input: gaussMult (1 :@ 2) (2 :@ 3)
 - - Expected Output:  (-4) :@ 7
 - - Acutal Output:  (-4) :@ 7
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 2
 - - Input: gaussMult ((-2) :+ (-2)) (2 :+ 2)
 - - Expected Output: 0 :+ (-8)
 - - Acutal Output: 0 :+ (-8)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussMult
 - - Test Case Number: 3
 - - Input: gaussMult ((-2) :@ 1) (1 :@ (-2))
 - - Expected Output: 0 :@ 5
 - - Acutal Output: 0 :@ 5
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 1
 - - Input: gaussNorm (2 :+ 3)
 - - Expected Output: 13
 - - Acutal Output: 13
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 2
 - - Input: gaussNorm ((-2) :@ 1) 
 - - Expected Output: 5
 - - Acutal Output: 5
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: gaussNorm
 - - Test Case Number: 3
 - - Input: gaussNorm (1 :+ (-2))
 - - Expected Output: 5
 - - Acutal Output: 5
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 1
 - - Input: maxGaussNorm [(1 :+ 2), (2 :+ 3), ((-3) :+ (-4))]
 - - Expected Output: (-3) :+ (-4)
 - - Acutal Output: (-3) :+ (-4)
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 2
 - - Input: maxGaussNorm [(1 :@ 1), (1 :@ 1)]
 - - Expected Output: 1 :@ 1
 - - Acutal Output: 1 :@ 1
 - -----------------------------------------------------------------
 - -----------------------------------------------------------------
 - - Function: maxGaussNorm
 - - Test Case Number: 3
 - - Input: maxGaussNorm [((-1) :@ (-2))]
 - - Expected Output: (-1) :@ (-2)
 - - Acutal Output: (-1) :@ (-2)
 - -----------------------------------------------------------------
 -
 -
 -}
