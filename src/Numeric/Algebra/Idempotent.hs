{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Numeric.Algebra.Idempotent 
  ( Band
  , pow1pBand
  , powBand
  ) where

import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Natural
import Data.Set (Set)
import Data.IntSet (IntSet)

-- | An multiplicative semigroup with idempotent multiplication.
--
-- > a * a = a
class Multiplicative r => Band r

pow1pBand :: r -> Natural -> r
pow1pBand r _ = r 

powBand :: Unital r => r -> Natural -> r
powBand _ 0 = one
powBand r _ = r

instance Band ()
instance Band Bool
instance (Band a, Band b) => Band (a,b)
instance (Band a, Band b, Band c) => Band (a,b,c)
instance (Band a, Band b, Band c, Band d) => Band (a,b,c,d)
instance (Band a, Band b, Band c, Band d, Band e) => Band (a,b,c,d,e)
