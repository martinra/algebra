{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, UndecidableInstances, DeriveDataTypeable #-}
module Numeric.CombinatorialFreeAlgebra.Hyperbolic
  ( HyperBasis'(..)
  ) where

import Control.Applicative
import Control.Monad.Reader.Class
import Data.Data
import Data.Distributive
import Data.Functor.Bind
import Data.Functor.Rep
import Data.Foldable
import Data.Ix
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Data.Semigroup
import Data.Traversable
import Numeric.Algebra
import Numeric.Coalgebra.Hyperbolic.Class
import Prelude hiding ((-),(+),(*),negate,subtract, fromInteger)

-- the dual hyperbolic basis
data HyperBasis' = Cosh' | Sinh' deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded,Data,Typeable)

instance Hyperbolic HyperBasis' where
  cosh = Cosh'
  sinh = Sinh'

instance Rig r => Hyperbolic (HyperBasis' -> r) where
  cosh Sinh' = zero
  cosh Cosh' = one
  sinh Sinh' = one
  sinh Cosh' = zero

instance Representable Hyper' where
  type Rep Hyper' = HyperBasis'
  tabulate f = Hyper' (f Cosh') (f Sinh')
  index (Hyper' a _ ) Cosh' = a
  index (Hyper' _ b ) Sinh' = b

instance MonadReader HyperBasis' Hyper' where
  ask = askRep
  local = localRep

-- the dual hyperbolic trigonometric algebra
instance (Commutative k, Semiring k) => CombinatorialFreeAlgebra k HyperBasis' where
  mult f = f' where
    fs = f Sinh' Cosh' + f Cosh' Sinh'
    fc = f Cosh' Cosh' + f Sinh' Sinh'
    f' Sinh' = fs
    f' Cosh' = fc

instance (Commutative k, Monoidal k, Semiring k) => UnitalCombinatorialFreeAlgebra k HyperBasis' where
  unit _ Sinh' = zero
  unit x Cosh' = x

-- the diagonal coalgebra
instance (Commutative k, Monoidal k, Semiring k) => CombinatorialFreeCoalgebra k HyperBasis' where
  comult f = f' where
     fs = f Sinh'
     fc = f Cosh'
     f' Sinh' Sinh' = fs
     f' Sinh' Cosh' = zero
     f' Cosh' Sinh' = zero
     f' Cosh' Cosh' = fc

instance (Commutative k, Monoidal k, Semiring k) => CounitalCombinatorialFreeCoalgebra k HyperBasis' where
  counit f = f Cosh' + f Sinh'

instance (Commutative k, Monoidal k, Semiring k) => CombinatorialFreeBialgebra k HyperBasis'

instance (Commutative k, Group k, InvolutiveSemiring k) => InvolutiveCombinatorialFreeAlgebra k HyperBasis' where
  inv f = f' where
    afc = adjoint (f Cosh')
    nfs = negate (f Sinh')
    f' Cosh' = afc
    f' Sinh' = nfs

instance (Commutative k, Group k, InvolutiveSemiring k) => InvolutiveCombinatorialFreeCoalgebra k HyperBasis' where
  coinv = inv

instance (Commutative k, Group k, InvolutiveSemiring k) => HopfCombinatorialFreeAlgebra k HyperBasis' where
  antipode = inv
