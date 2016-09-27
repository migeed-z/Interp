{-# LANGUAGE GADTs #-}

module Interp where

data Term a where

  Num :: Int
      -> Term Int
  Bool:: Bool
      -> Term Bool
  Add :: Term Int
      -> Term Int
      -> Term Int
  Gt  :: Term Int
      -> Term Int
      -> Term Bool
  If  :: Term Bool
      -> Term a
      -> Term a
      -> Term a


interp :: Term a -> a
interp (Num i)   = i
interp (Bool i)  = i
interp (Add x y) = interp x + interp y
interp (Gt  x y) = interp x > interp y
interp (If x y z) = if (interp x) then (interp y) else (interp z)


test0 :: [Bool]

test0 = [ 1     == interp(Num 1),
          False == interp(Bool False),
          3     == interp(Add (Num 1) (Num 2)),
          True  == interp(Gt (Num 3) (Num 2)),
          True  == interp(If (Bool True) (Bool True) (Bool False)),
          3     == interp(If (Bool False) (Num 1) (Num 3))]
