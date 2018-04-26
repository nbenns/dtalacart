module Algebra.Expr where

data Expr f = In (f (Expr f))

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

