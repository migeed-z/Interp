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

lookupEnv :: String -> Env a ->  a
lookupEnv n [] = error("Unbound variable " ++ n)
lookupEnv n ((name, val):env) = if n == name then val else lookupEnv n env


interp :: Term a ->  Env b -> a
interp (Num i) _   = i
interp (Bool i) _  = i
interp (Add x y) env = (interp x env) + (interp y env)
interp (Gt  x y) env = (interp x env) > (interp y env)
interp (If x y z)env = if (interp x env) then (interp y env) else (interp z env)


