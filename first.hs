data Expr = Val Int | Add Expr Expr

eval :: Expr -> Int
eval (Val x) = x
eval (Add x y) = eval x + eval y

render :: Expr -> String
render (Val x) = show x
render (Add x y) = "(" ++ render x ++ " + " ++ render y ++ ")"

main = do
  let e = Add (Add (Val 2) (Val 1)) (Val 3)
  putStrLn $ render e
  print $ eval e
