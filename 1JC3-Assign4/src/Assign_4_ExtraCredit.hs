{-# LANGUAGE FlexibleContexts,FlexibleInstances,IncoherentInstances #-}
{- Assignment 4 Extra Credit
 - Name: Frank Yang
 - Date: Nov 15
 -}
module Assign_4_ExtraCredit where

macid :: String
macid = "yangf51"
   
data MathExpr a = X
                | Coef a
                | Sum (MathExpr a) (MathExpr a)
                | Prod (MathExpr a) (MathExpr a)
                | Quot (MathExpr a) (MathExpr a)
                | Exp (MathExpr a)
                | Log (MathExpr a)
                deriving (Eq,Show,Read)

{- -----------------------------------------------------------------
- value
- -----------------------------------------------------------------
- Similar to Assignment 3, we simply use pattern matching to find the value of a MathExpr
-}

value :: (Floating a, Eq a) => MathExpr a -> a -> a
value X n = n
value (Coef a) _ = a
value (Sum s1 s2) n = (value s1 n) + (value s2 n)
value (Prod s1 s2) n = (value s1 n) * (value s2 n)
value (Quot s1 s2) n = (value s1 n) / (value s2 n) 
value (Exp s1) n = exp(value s1 n)
value (Log s1) n = log(value s1 n)

{- -----------------------------------------------------------------
- simp
- -----------------------------------------------------------------
- Description: Utilizes Pattern matching to simplify a MathExpr (Possibly recursively) following the 9 given rules in the assignment
-}
simp :: (Floating a, Eq a) => MathExpr a -> MathExpr a
simp (Sum (Coef  0.0) u) = simp u
simp (Sum u (Coef  0.0)) = simp u
simp (Sum (Coef a) (Coef b)) = Coef(a+b)
simp (Prod (Coef a) (Coef b))  = Coef(a*b)
simp (Quot (Coef a) (Coef b))  = Coef(a/b)
simp (Exp (Coef a))  = Coef(exp(a))
simp (Log (Coef a))  = Coef(log(a))
simp (Sum u v) =
    let 
        u' = simp u
        v' = simp v
    in
        if u' == u && v' == v 
            then (Sum u v) 
            else  simp (Sum u' v')
simp (Prod (Coef  0.0) u) = (Coef 0)
simp (Prod u (Coef  0.0)) = (Coef 0)
simp (Prod u (Coef 1)) = simp u
simp (Prod (Coef 1) u) = simp u
simp (Prod u v) =
    let
        u' = simp u
        v' = simp v
    in
        if u' == u && v' == v 
            then (Prod u v) 
            else  simp (Prod u' v')
simp (Quot u (Coef  1)) = simp u
simp (Quot u v) =
    let
        u' = simp u
        v' = simp v
    in
        if u' == u && v' == v 
            then (Quot u v) 
            else  simp (Quot u' v')
simp (Exp (Coef 0)) = Coef 1
simp (Log (Coef 1)) = Coef 0
simp u = u


{- -----------------------------------------------------------------
- diff
- -----------------------------------------------------------------
- Description: Uses diffrentiation rules given above to diffrentiate a MathExpr with pattern matching and recursion
-}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = (Coef 1)
diff (Coef _) = (Coef 0) 
diff (Sum u v) = (Sum (diff u) (diff v))
diff (Prod u v) = (Sum (Prod (diff u) v) (Prod u (diff v)))
diff (Quot u v) = (Quot (Sum (Prod (diff u) v) (Prod (Prod (Coef (-1)) u) (diff v))) (Prod v v))
diff (Exp u) = (Prod (Exp u) (diff u))
diff (Log u) = (Quot (diff u) u)

{- -----------------------------------------------------------------
- readDiffWrite
- -----------------------------------------------------------------
- Description: Uses IO inside do. We first read the file, then we cast it into a MathExpr Double with read
- Then we simplify the diffrentiation of the MathExpr, finally converting it back into a string and writing it to the end file g
-}
readDiffWrite :: FilePath -> FilePath -> IO ()
readDiffWrite f g = 
    do u <- readFile f 
       let a = read u :: MathExpr Double
       let b = simp(diff a)
       let c = show b
       writeFile g (c ++ "\n")