{-# Language TypeOperators #-}

data (f :+: g) e = Inl (f e) | Inr (g e)

data Expr f = In (f (Expr f))
data Val e = Val Int
data Add e = Add e e

type Algebra = Val :+: Add

eval :: Expr Algebra -> Int
eval (In (Inl (Val n))) = n
eval (In (Inr (Add x y))) = eval x + eval y

render :: Expr Algebra -> String
render (In (Inl (Val n))) = show n
render (In (Inr (Add x y))) = "(" ++ render x ++ " + " ++ render y ++ ")"

addExample :: Expr Algebra
addExample = In (Inr (Add (In (Inl (Val 118))) (In (Inl (Val 1219)))))

main = do
  print $ render addExample
  print $ eval addExample

