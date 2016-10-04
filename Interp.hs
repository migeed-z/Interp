{-# LANGUAGE GADTs #-}

module Interp where

data Term env a where

  Num :: Int
      -> Term env Int  
  Bool:: Bool
      -> Term env Bool
  Add :: Term env Int
      -> Term env Int
      -> Term env Int
  Gt  :: Term env Int
      -> Term env Int
      -> Term env Bool
  If  :: Term env Bool
      -> Term env a
      -> Term env a
      -> Term env a
  Var :: VarT env a
      -> Term env a
  Lam :: Term (a, env) b
      -> Term env (a -> b)
  App :: Term env (a -> b)
      -> Term env a
      -> Term env b
  
data VarT env b where
  Z :: VarT (b, env') b
  S :: VarT env' b
    -> VarT (c, env') b

  
interp :: Term env a -> env ->  a
interp (Num i) env   = i
interp (Bool i) env  = i
interp (Add x y) env = (interp x env) + (interp y env)
interp (Gt  x y) env= (interp x env) > (interp y env)
interp (If x y z) env= if (interp x env) then (interp y env) else (interp z env)
interp (Var x) env = lookupEnv x env
interp (Lam e) env = \x -> interp e (x, env)
interp (App f x) env = interp f env (interp x env)

lookupEnv :: VarT env a
          -> env
          -> a
lookupEnv Z (a, env) = a
lookupEnv (S x) (b, env) = lookupEnv x env

test0 :: [Bool]

test0 = [ 1     == interp(Num 1) (),
          False == interp(Bool False) (),
          3     == interp(Add (Num 1) (Num 2)) (),
          True  == interp(Gt (Num 3) (Num 2)) (),
          True  == interp(If (Bool True) (Bool True) (Bool False)) (),
          3     == interp(If (Bool False) (Num 1) (Num 3)) ()]
