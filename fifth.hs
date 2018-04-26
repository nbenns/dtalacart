{-# Language TypeOperators, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

infixr 6 :+:
data (f :+: g) e = Inl (f e) | Inr (g e)

data Expr f = In (f (Expr f))
data Val e = Val Int
data Add e = Add e e
data Mul x = Mul x x

type Algebra = Val :+: Add :+: Mul

instance Functor Val where
  fmap f (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance Functor Mul where
  fmap f (Mul e1 e2) = Mul (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

class Functor f => Eval f where
  evalAlgebra :: f Int -> Int

instance Eval Val where
  evalAlgebra (Val x) = x

instance Eval Add where
  evalAlgebra (Add x y) = x + y

instance Eval Mul where
  evalAlgebra (Mul x y) = x * y

instance (Eval f, Eval g) => Eval (f :+: g) where
  evalAlgebra (Inl x) = evalAlgebra x
  evalAlgebra (Inr y) = evalAlgebra y

class Render f where
  render :: Render g => f (Expr g) -> String

instance Render Val where
  render (Val n) = show n

instance Render Add where
  render (Add x y) = "(" ++ pretty x ++ " + " ++ pretty y ++ ")"

instance Render Mul where
  render (Mul x y) = "(" ++ pretty x ++ " * " ++ pretty y ++ ")"

instance (Render f, Render g) => Render (f :+: g) where
  render (Inl x) = render x
  render (Inr y) = render y

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

val :: (Val :<: f) => Int -> Expr f
val x = inject (Val x)

infixl 6 <+>
(<+>) :: (Add :<: f ) => Expr f -> Expr f -> Expr f
x <+> y = inject (Add x y)

infixl 7 <@>
(<@>) :: (Mul :<: f ) => Expr f -> Expr f -> Expr f
x <@> y = inject (Mul x y)

foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)

distr :: (Add :<: f, Mul :<: f) => Expr f -> Maybe (Expr f)
distr t = do
  Mul a b <- match t
  Add c d <- match b
  return (a <@> c <+> a <@> d)

eval :: Eval f => Expr f -> Int
eval expr = foldExpr evalAlgebra expr

pretty :: Render f => Expr f -> String
pretty (In t) = render t

addExample :: Expr Algebra
addExample = val 80 <@> val 5 <+> val 4

main = do
  print $ pretty addExample
  print $ eval addExample
