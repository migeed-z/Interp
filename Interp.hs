{-# LANGUAGE GADTs #-}

module Interp where

--data Sty ty  where
  --TBool :: Sty Bool
  --TInt  :: Sty Int


data Term a where
  Num :: Int
      -> Term Int
  Add :: Term Int
      -> Term Int
      -> Term Int
  Gt  :: Term Int
      -> Term Int
      -> Term Bool
      
    
interp :: Term a -> a
interp (Num i) = i
interp (Add x y) = interp x + interp y
interp (Gt  x y) = interp x > interp y




















