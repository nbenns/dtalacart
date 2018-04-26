{-# Language TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

data (f :+: g) e = Inl (f e) | Inr (g e)

data Expr f = In (f (Expr f))
data Val e = Val Int
data Add e = Add e e

type Algebra = Val :+: Add

instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance (Eval f ,Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance {-# Overlaps #-} (Functor f, Functor g) => f :<: (f :+: g) where
  inj = Inl

instance (Functor f , Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = Inr . inj

inject :: (g :<: f ) => g (Expr f ) -> Expr f
inject = In . inj

val :: (Val :<: f ) => Int -> Expr f
val x = inject (Val x)

(<+>) :: (Add :<: f ) => Expr f -> Expr f -> Expr f
x <+> y = inject (Add x y)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

render :: Expr Algebra -> String
render (In (Inl (Val n))) = show n
render (In (Inr (Add x y))) = "(" ++ render x ++ " + " ++ render y ++ ")"

addExample :: Expr Algebra
addExample = val 30000 <+> val 1330 <+> val 7

main = do
  print $ render addExample
  print $ eval addExample
