{-# Language TypeOperators, FlexibleContexts #-}
module Algebra.Mul where

import Algebra.Expr
import Algebra.Subtype

data Mul x = Mul x x
  deriving(Show)

instance Functor Mul where
  fmap f (Mul e1 e2) = Mul (f e1) (f e2)

infixl 7 <@>
(<@>) :: (Mul :<: f ) => Expr f -> Expr f -> Expr f
x <@> y = inject (Mul x y)

