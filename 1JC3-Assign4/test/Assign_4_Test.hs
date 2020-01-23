{- Assignment 4 Tests
 - Name: Frank Yang
 - Date: Nov 15th 2019
 -}

import Assign_4
import Assign_4_ExtraCredit

import Test.QuickCheck
--import Criterion.Main   -- see https://www.stackage.org/haddock/lts-8.24/criterion-1.1.4.0/Criterion-Main.html

main :: IO ()
main = do print "Performing Test 1: "
          quickCheck prop1
          quickCheck propValue
          quickCheck propDiff
          quickCheck propSimp

prop1 :: Int -> Bool
prop1 _ = True

propValue :: Double -> Bool
propValue a = value (Sum X (Sum X X)) a == value (Prod (Coef 3) X ) a

propDiff :: Double -> Bool
propDiff a = simp(diff(Prod X (Coef a))) == (Coef a)

propSimp :: Double -> Bool
propSimp a = simp (Prod (Coef a) (Sum (Coef 0) X)) == simp (Prod  (Coef a) X)



