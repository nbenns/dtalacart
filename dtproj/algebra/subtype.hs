{-# Language TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}
module Algebra.Subtype where

import Algebra.Coproduct
import Algebra.Expr

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)

instance Functor f => f :<: f where
  inj = id
  prj = Just

instance {-# Overlaps #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl
  prj (Inl a) = Just a
  prj (Inr a) = Nothing

instance (Functor f , Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj
  prj (Inr a) = prj a 
  prj (Inl a) = Nothing

inject :: (g :<: f ) => g (Expr f ) -> Expr f
inject = In . inj

match :: (g :<: f) => Expr f -> Maybe (g (Expr f))
match (In t) = prj t

