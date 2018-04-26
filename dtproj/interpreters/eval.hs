{-# Language TypeOperators #-}
module Interpreters.Eval where

import Algebra

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr 

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y
