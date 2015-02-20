{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.CombinatorialFreeAlgebra.Division
  ( DivisionCombinatorialFreeAlgebra(..)
  ) where

import Prelude hiding ((*), recip, (/),(^))
import Numeric.Algebra.Division
import Numeric.Algebra.Unital
import Numeric.CombinatorialFreeAlgebra.Unital


class UnitalCombinatorialFreeAlgebra r a => DivisionCombinatorialFreeAlgebra r a where
  recipriocal :: (a -> r) -> a -> r
  -- recipriocal f = one `over` f

instance (Unital r, DivisionCombinatorialFreeAlgebra r a) => Division (a -> r) where
  recip = recipriocal

