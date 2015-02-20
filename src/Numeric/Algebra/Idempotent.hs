{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Numeric.Algebra.Idempotent 
  ( Band
  , pow1pBand
  , powBand
  -- * Idempotent algebras
  , IdempotentCombinatorialFreeAlgebra
  , IdempotentCombinatorialFreeCoalgebra
  , IdempotentBialgebra
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

-- idempotent algebra
class CombinatorialFreeAlgebra r a => IdempotentCombinatorialFreeAlgebra r a
instance (Semiring r, Band r, Ord a) => IdempotentCombinatorialFreeAlgebra r (Set a)
instance (Semiring r, Band r) => IdempotentCombinatorialFreeAlgebra r IntSet
instance (Semiring r, Band r) => IdempotentCombinatorialFreeAlgebra r ()
instance (IdempotentCombinatorialFreeAlgebra r a, IdempotentCombinatorialFreeAlgebra r b) => IdempotentCombinatorialFreeAlgebra r (a,b)
instance (IdempotentCombinatorialFreeAlgebra r a, IdempotentCombinatorialFreeAlgebra r b, IdempotentCombinatorialFreeAlgebra r c) => IdempotentCombinatorialFreeAlgebra r (a,b,c)
instance (IdempotentCombinatorialFreeAlgebra r a, IdempotentCombinatorialFreeAlgebra r b, IdempotentCombinatorialFreeAlgebra r c, IdempotentCombinatorialFreeAlgebra r d) => IdempotentCombinatorialFreeAlgebra r (a,b,c,d)
instance (IdempotentCombinatorialFreeAlgebra r a, IdempotentCombinatorialFreeAlgebra r b, IdempotentCombinatorialFreeAlgebra r c, IdempotentCombinatorialFreeAlgebra r d, IdempotentCombinatorialFreeAlgebra r e) => IdempotentCombinatorialFreeAlgebra r (a,b,c,d,e)

-- idempotent coalgebra
class CombinatorialFreeCoalgebra r c => IdempotentCombinatorialFreeCoalgebra r c
instance (Semiring r, Band r, Ord c) => IdempotentCombinatorialFreeCoalgebra r (Set c)
instance (Semiring r, Band r) => IdempotentCombinatorialFreeCoalgebra r IntSet
instance (Semiring r, Band r) => IdempotentCombinatorialFreeCoalgebra r ()
instance (IdempotentCombinatorialFreeCoalgebra r a, IdempotentCombinatorialFreeCoalgebra r b) => IdempotentCombinatorialFreeCoalgebra r (a,b)
instance (IdempotentCombinatorialFreeCoalgebra r a, IdempotentCombinatorialFreeCoalgebra r b, IdempotentCombinatorialFreeCoalgebra r c) => IdempotentCombinatorialFreeCoalgebra r (a,b,c)
instance (IdempotentCombinatorialFreeCoalgebra r a, IdempotentCombinatorialFreeCoalgebra r b, IdempotentCombinatorialFreeCoalgebra r c, IdempotentCombinatorialFreeCoalgebra r d) => IdempotentCombinatorialFreeCoalgebra r (a,b,c,d)
instance (IdempotentCombinatorialFreeCoalgebra r a, IdempotentCombinatorialFreeCoalgebra r b, IdempotentCombinatorialFreeCoalgebra r c, IdempotentCombinatorialFreeCoalgebra r d, IdempotentCombinatorialFreeCoalgebra r e) => IdempotentCombinatorialFreeCoalgebra r (a,b,c,d,e)

-- idempotent bialgebra
class (Bialgebra r h, IdempotentCombinatorialFreeAlgebra r h, IdempotentCombinatorialFreeCoalgebra r h) => IdempotentBialgebra r h 
instance (Bialgebra r h, IdempotentCombinatorialFreeAlgebra r h, IdempotentCombinatorialFreeCoalgebra r h) => IdempotentBialgebra r h 
