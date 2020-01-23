{- Assignment 3
 - Name: Frank Yang
 - Date: Oct 23 2019
 -}
module Assign_3 where

import qualified Data.Map.Strict as IM

macid :: String
macid = "yangf51"

data Poly a = X
            | Coef a
            | Sum (Poly a) (Poly a)
            | Prod (Poly a) (Poly a)
  deriving (Show)


newtype PolyList a = PolyList [a]
  deriving (Show)

{- -----------------------------------------------------------------
 - polyListConc
 - -----------------------------------------------------------------
 - Description: This Concatenates the two PolyLists, used as a utility function
 -}

polyListConc :: PolyList a -> PolyList a -> PolyList a
polyListConc (PolyList (xs)) (PolyList (ys)) = PolyList (xs++ys)
 
{- -----------------------------------------------------------------
  - polyListMap
  - -----------------------------------------------------------------
  - Description: This maps a number across PolyList - used as a utility function
  -}
 
polyListMap :: (Num a,Eq a) => a -> PolyList a -> PolyList a
polyListMap _ (PolyList []) = (PolyList [])
polyListMap a (PolyList (x:xs)) = polyListConc (PolyList [a*x]) (polyListMap a (PolyList xs)) 
 

{- -----------------------------------------------------------------
 - polyValue
 - -----------------------------------------------------------------
 - Description: Find a polynomial p at x = n, this is done through recursively breaking down the Polynomial p 
 - according to it's definition 
 -}
polyValue :: (Num a) => Poly a -> a -> a
polyValue X n = n
polyValue (Coef a) _ = a
polyValue (Sum p1 p2) n = (polyValue p1 n) + (polyValue p2 n)
polyValue (Prod p1 p2) n = polyValue p1 n * polyValue p2 n

{- -----------------------------------------------------------------
 - polyListValue
 - -----------------------------------------------------------------
 - Description: Uses Horner's method to recursively solve the polynomial when plugging in a value n
 - a0+x(a1+x(a2++x(am)))
 -}
polyListValue :: (Num a,Eq a) => PolyList a -> a -> a
polyListValue (PolyList []) _ = error ("0 polynomial")
polyListValue (PolyList [p]) n = p
polyListValue (PolyList (a:as)) n = a + (n * polyListValue (PolyList as) n)


{- -----------------------------------------------------------------
 - polyListSum
 - -----------------------------------------------------------------
 - Description: We add the first element of each list together, continue down for every other element. When one
 - list is longer than the other, we add 0s to the longer list instead
 -}
polyListSum :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListSum (PolyList []) (PolyList []) = (PolyList [])
polyListSum (PolyList (p:p1)) (PolyList (q:q1)) = polyListConc (PolyList [p + q]) (polyListSum (PolyList p1) (PolyList q1))
polyListSum (PolyList (p:p1)) (PolyList []) = polyListConc (PolyList [p]) (polyListSum (PolyList p1) (PolyList []))
polyListSum (PolyList []) (PolyList(q:q1)) = polyListConc (PolyList [q]) (polyListSum (PolyList []) (PolyList q1))


{- -----------------------------------------------------------------
 - polyListDegree
 - -----------------------------------------------------------------
 - Description: Recursively determines the length of the PolyList, adds one every time we remove the head
 -}
polyListDegree :: (Num a,Eq a) => PolyList a -> Integer
polyListDegree (PolyList []) = error("undefined")
polyListDegree (PolyList [a]) = 0
polyListDegree (PolyList (p:p1)) = 1 + (polyListDegree (PolyList p1))


{- -----------------------------------------------------------------
 - polyListProd
 - -----------------------------------------------------------------
 - Description: We expand the two PolyLists, and shift each expansion to create multiple lists that have matching degrees. We then add all the lists together
 - [1,2,3] * [1,2]
 - [1,2,0,0]
 - [0,2,4,0]
 - [0,0,3,6]
 - GIVES
 - [1,4,7,6]
 -}
polyListProd :: (Num a,Eq a) => PolyList a -> PolyList a -> PolyList a
polyListProd (PolyList []) _ = (PolyList [])
polyListProd _ (PolyList []) = (PolyList [])
polyListProd _ (PolyList [0]) = (PolyList [0])
polyListProd (PolyList [0]) _ = (PolyList [0])
polyListProd (PolyList (p:p1)) (PolyList p2) = polyListSum (polyListMap p (PolyList p2)) (polyListConc (PolyList [0]) (polyListProd (PolyList p1) (PolyList p2))) 


{- -----------------------------------------------------------------
 - polyListToPoly
 - -----------------------------------------------------------------
 - Description: We use the PolyList and undo it recursively through reducing it with a1 + x(a2 + x(a3 +x(...))) {using Poly Notation}
 -}
polyListToPoly :: Num a => PolyList a -> Poly a
polyListToPoly (PolyList [p]) = Coef p
polyListToPoly (PolyList []) = error ("0 polynomial")
polyListToPoly  (PolyList (p:p1)) = Sum (Coef p) (Prod X (polyListToPoly (PolyList p1)))

{- -----------------------------------------------------------------
 - polyToPolyList
 - -----------------------------------------------------------------
 - Description: Breaks down the Poly with pattern matching and recursively builds a PolyList using X and Coef a as base cases
 -}
polyToPolyList :: (Num a,Eq a) => Poly a -> PolyList a
polyToPolyList X = (PolyList [0,1])
polyToPolyList (Coef a) = (PolyList[a])
polyToPolyList (Sum a b) = polyListSum(polyToPolyList a) (polyToPolyList b)
polyToPolyList (Prod a b) = polyListProd (polyToPolyList a) (polyToPolyList b)

{- -----------------------------------------------------------------
Test Cases:
 - -----------------------------------------------------------------

 Function: polyValue
 Test Case Number: 1
 Input: polyValue (Coef 3) 2
 Expected Output: 3
 Actual Output: 3

 Function: polyValue
 Test Case Number: 2
 Input: polyValue (Sum (Prod X X) (Coef 2)) 2
 Expected Output: 6
 Actual Output: 6

 Function: polyValue
 Test Case Number: 3
 Input: polyValue X 3
 Expected Output: 3
 Actual Output: 3
--------------------------------------------------
 Function: polyListValue
 Test Case Number: 1
 Input: polyListValue (PolyList [1,2,3]) 3
 Expected Output: 34
 Actual Output: 34

 Function: polyListValue
 Test Case Number: 2
 Input: polyListValue (PolyList [1,2,3,4,5]) 2
 Expected Output: 129
 Actual Output: 129

 Function: polyListValue
 Test Case Number: 3
 Input: polyListValue (PolyList []) 2
 Expected Output: Exception: 0 polynomial
 Actual Output: Exception: 0 polynomial
--------------------------------------------------
 Function: polyListSum
 Test Case Number: 1
 Input: polyListSum (PolyList [1,2,3]) (PolyList [1,2,3])
 Expected Output: PolyList [2,4,6]
 Actual Output: PolyList [2,4,6]
 
 Function: polyListSum
 Test Case Number: 2
 Input: (PolyList [1,2,3]) (PolyList [1,2])
 Expected Output: PolyList [2,4,3]
 Actual Output: PolyList [2,4,3]

 Function: polyListSum
 Test Case Number: 3
 Input: polyListSum (PolyList [-1,2,3]) (PolyList [])
 Expected Output: PolyList [-1,2,3]
 Actual Output: PolyList [-1,2,3]
--------------------------------------------------
 Function: polyListDegree
 Test Case Number: 1
 Input: polyListDegree (PolyList[0])
 Expected Output: 0
 Actual Output: 0

 Function: polyListDegree
 Test Case Number: 2
 Input: polyListDegree (PolyList[0,1,2,3])
 Expected Output: 3
 Actual Output: 3

 Function: polyListDegree
 Test Case Number: 3
 Input: polyListDegree (PolyList[])
 Expected Output: Exception: undefined
 Actual Output: Exception: undefined
--------------------------------------------------
 Function: polyListProd
 Test Case Number: 1
 Input: polyListProd (PolyList[1,2,3]) (PolyList [1,2,3])
 Expected Output: PolyList [1,4,10,12,9]
 Actual Output: PolyList [1,4,10,12,9]

 Function: polyListProd
 Test Case Number: 2
 Input: polyListProd (PolyList[1,2,3]) (PolyList [])
 Expected Output: PolyList []
 Actual Output: PolyList []

 Function: polyListProd
 Test Case Number: 3
 Input: polyListProd (PolyList[1,2,3]) (PolyList [0])
 Expected Output: PolyList [0]
 Actual Output: PolyList [0]
--------------------------------------------------
 Function: polyListToPoly
 Test Case Number: 1
 Input: polyListToPoly (PolyList[1,2,3,4,5])
 Expected Output: Sum (Coef 1) (Prod X (Sum (Coef 2) (Prod X (Sum (Coef 3) (Prod X (Sum (Coef 4) (Prod X (Coef 5))))))))
 Actual Output: Sum (Coef 1) (Prod X (Sum (Coef 2) (Prod X (Sum (Coef 3) (Prod X (Sum (Coef 4) (Prod X (Coef 5))))))))

 Function: polyListToPoly
 Test Case Number: 2
 Input:  polyListToPoly (PolyList[1,0,02])
 Expected Output: Sum (Coef 1) (Prod X (Sum (Coef 0) (Prod X (Coef 2))))
 Actual Output: Sum (Coef 1) (Prod X (Sum (Coef 0) (Prod X (Coef 2))))

 Function: polyListToPoly
 Test Case Number: 3
 Input: polyListToPoly (PolyList[])
 Expected Output: Exception: 0 polynomial
 Actual Output: Exception: 0 polynomial

--------------------------------------------------
 Function: polyToPolyList
 Test Case Number: 1
 Input: polyToPolyList (Coef 2)
 Expected Output: PolyList [2]
 Actual Output: PolyList [2]

 Function: polyToPolyList
 Test Case Number: 2
 Input: polyToPolyList (Sum (Coef 1) (Prod X (Sum (Coef 2) (Prod X (Sum (Coef 3) (Prod X (Sum (Coef 4) (Prod X (Coef 5)))))))))
 Expected Output: PolyList [1,2,3,4,5]
 Actual Output: PolyList [1,2,3,4,5]

 Function: polyToPolyList
 Test Case Number: 3
 Input: polyToPolyList X
 Expected Output: PolyList [0,1]
 Actual Output: PolyList [0,1]
-}