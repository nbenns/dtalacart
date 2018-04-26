{-# Language TypeOperators #-}

module Algebra (
  module Algebra.Expr,
  module Algebra.Coproduct,
  module Algebra.Subtype,
  module Algebra.Val,
  module Algebra.Add,
  module Algebra.Mul,
  Algebra,
) where

import Algebra.Expr
import Algebra.Coproduct
import Algebra.Subtype
import Algebra.Val
import Algebra.Add
import Algebra.Mul

type Algebra = Val :+: Add :+: Mul
