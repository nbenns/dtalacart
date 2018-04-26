{-# Language TypeOperators, FlexibleContexts #-}
module Algebra.Val where

import Algebra.Expr
import Algebra.Subtype

data Val e = Val Int
  deriving(Show)

instance Functor Val where
  fmap f (Val x) = Val x

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

