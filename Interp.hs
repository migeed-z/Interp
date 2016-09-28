{-# LANGUAGE GADTs #-}

module Interp where

data Term a where

  Num :: Int
      -> Term Int
  Bool:: Bool
      -> Term Bool
  Var :: Term String
      -> Term a
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
  Lam :: String
      -> Term a
      -> Term a
  App :: Term a
      -> Term a
      -> Term a

type Env a = [(String, a)]

env0 :: Env a
env0 = []

lookupEnv                     :: String -> Env a ->  a
lookupEnv n []                = error("Unbound variable " ++ n)
lookupEnv n ((name, val):env) = if n == name then val else lookupEnv n env


interp               :: Term a ->  Env b -> a
interp (Num i) _     = i
interp (Bool i) _    = i
interp (Add x y) env = (interp x env) + (interp y env)
interp (Gt  x y) env = (interp x env) > (interp y env)
interp (If x y z)env = if (interp x env) then (interp y env) else (interp z env)

test0 :: [Bool]
test0 = [ 1     == interp (Num 1) env0,
          False == interp (Bool False) env0,
          3     == interp (Add (Num 1) (Num 2)) env0,
          True  == interp (Gt (Num 3) (Num 2)) env0,
          True  == interp (If (Bool True) (Bool True) (Bool False)) env0,
          3     == interp (If (Bool False) (Num 1) (Num 3)) env0]
