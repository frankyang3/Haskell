{- Assignment 5 Tests
 - Name: Frank Yang
 - Date: Nov 29th 2019
 -}

import Assign_5
import Assign_5_ExtraCredit

import Test.QuickCheck

main :: IO ()
main = do print "Performing Test 1: "
          quickCheck prop1
          

prop1 :: Int -> Bool
prop1 _ = True

propDefiniteIntegral :: Integer -> Bool
propDefiniteIntegral k = if k > 0 then (definiteIntegral 0 2 (\x-> x**2) k) ==  (definiteIntegral 0 2 (\x -> (x*x)) k) else True

propFunH :: Integer -> Bool
propFunH n = if n > 0 then funH n == definiteIntegral 0 1 (\x -> x**(1/ fromInteger n)-x**( fromInteger n)) 10000 else True

propFunK :: Double -> Bool
propFunK k = if k >0 then funK k == definiteIntegral (-1) 1 (\x -> k**x) 10000 else True

propUniformToNorm :: Double -> Bool
propUniformToNorm x  = if log(x)<0 then uniformToNorm x 1 == (sqrt((-2)*log( x)) * cos(2*pi*1), sqrt((-2)*log( x)) * sin(2*pi*1)) else True

propUnitCircleHit :: Double -> Bool
propUnitCircleHit  a = unitCircleHit (1, a) == if (1**2+a**2 <= 1) then True else False
