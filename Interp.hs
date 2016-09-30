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

data VarT env a where
  Z :: VarT (b, env') b
  S :: VarT env' b
    -> VarT (c, end') b

  
interp :: Term env a -> a
interp (Num i)   = i
interp (Bool i)  = i
interp (Add x y) = interp x + interp y
interp (Gt  x y) = interp x > interp y
interp (If x y z) = if (interp x) then (interp y) else (interp z)


-- test0 :: [Bool]

-- test0 = [ 1     == interp(Num 1),
--           False == interp(Bool False),
--           3     == interp(Add (Num 1) (Num 2)),
--           True  == interp(Gt (Num 3) (Num 2)),
--           True  == interp(If (Bool True) (Bool True) (Bool False)),
--           3     == interp(If (Bool False) (Num 1) (Num 3))]
