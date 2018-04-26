{-# Language TypeOperators, FlexibleContexts #-}
module Algebra.Add where

import Algebra.Expr
import Algebra.Subtype

data Add e = Add e e
  deriving(Show)

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

infixl 6 <+>
(<+>) :: (Add :<: f ) => Expr f -> Expr f -> Expr f
x <+> y = inject (Add x y)

