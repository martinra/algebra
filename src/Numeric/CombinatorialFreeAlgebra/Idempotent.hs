{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module Numeric.CombinatorialFreeAlgebra.Idempotent 
  ( -- * Idempotent algebras
    IdempotentCombinatorialFreeAlgebra
  , IdempotentCombinatorialFreeCoalgebra
  , IdempotentCombinatorialFreeBialgebra
  ) where

import Numeric.Algebra.Class
import Numeric.Algebra.Idempotent
import Numeric.CombinatorialFreeAlgebra.Class
import Numeric.CombinatorialFreeAlgebra.Unital
import Data.Set (Set)
import Data.IntSet (IntSet)


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
class (CombinatorialFreeBialgebra r h, IdempotentCombinatorialFreeAlgebra r h, IdempotentCombinatorialFreeCoalgebra r h) => IdempotentCombinatorialFreeBialgebra r h 
instance (CombinatorialFreeBialgebra r h, IdempotentCombinatorialFreeAlgebra r h, IdempotentCombinatorialFreeCoalgebra r h) => IdempotentCombinatorialFreeBialgebra r h 
