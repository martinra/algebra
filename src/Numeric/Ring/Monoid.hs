{-# LANGUAGE DeriveDataTypeable
module Numeric.Ring.Monoid
  ( Mon(..)
  ) where

import Data.Data
import Data.Ix
import Numeric.Algebra

-- additive monoid ring
newtype Mon m = Mon m deriving 
  ( Eq, Ord, Show, Read, Ix, Enum, Bounded, Num, Real, Integral
  , Additive, Abelian, Idempotent, Partitionable
  , LeftModule Natural, RightModule Natural, Monoidal
  , LeftModule Integer, RightModule Integer, Group
  , Data, Typeable
  ) where

runMon :: Mon m -> m
runMon (Mon m) = m

instance HasTrie m => HasTrie (Mon m) where
  type BaseTrie (Mon m) = BaseTrie m
  embedKey = embedKey . runMon
  projectKey = Mon . projectKey

instance (Semiring r, Additive m) => CombinatorialFreeCoalgebra r (Mon m) where
  comult f m n = f (m + n)

instance (Semiring r, Monoidal m) => CounitalCombinatorialFreeCoalgebra r (Mon m) where
  counit f = f zero
  
instance (Commutative r, Semiring r, Abelian m) => CocommutativeCombinatorialFreeCoalgebra r (Mon m)

-- TODO: check
instance (IdempotentSemiring r, Idempotent m) => IdempotentCombinatorialFreeCoalgebra r (Mon m) 

instance (Semiring r, Partitionable m) => CombinatorialFreeAlgebra r (Mon m) where
  mult f = sum1 . partitionWith f

instance (Monoidal r, Semiring r, Partitionable m, DecidableZero m) => UnitalCombinatorialFreeAlgebra r (Mon m) where
  unit x m | isZero m  = x
           | otherwise = zero

instance (Commutative r, Semiring r, Partitionable m, Abelian m) => CommutativeCombinatorialFreeCoalgebra r (Mon m)

instance (IdempotentSemiring r, Partitionable m, Idempotent m) => IdempotentCombinatorialFreeAlgebra r (Mult m)

