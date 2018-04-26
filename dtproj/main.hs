{-# Language TypeOperators, FlexibleContexts #-}

import Algebra
import Interpreters.Render
import Interpreters.Eval

addExample :: Expr Algebra
addExample = val 80 <@> val 5 <+> val 4

main = do
  print $ pretty addExample
  print $ eval addExample
