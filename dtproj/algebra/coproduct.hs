{-# Language TypeOperators #-}
module Algebra.Coproduct where

infixr 6 :+:
data (f :+: g) e = Inl (f e) | Inr (g e)
  deriving(Show)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

