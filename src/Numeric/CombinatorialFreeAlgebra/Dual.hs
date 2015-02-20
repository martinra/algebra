{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, UndecidableInstances, DeriveDataTypeable #-}
module Numeric.CombinatorialFreeAlgebra.Dual
  ( Distinguished(..)
  , Infinitesimal(..)
  , DualBasis(..)
  , Dual(..)
  ) where

import Control.Applicative
import Control.Monad.Reader.Class
import Data.Data
import Data.Distributive
import Data.Functor.Bind
import Data.Functor.Rep
import Data.Foldable
import Data.Ix
import Data.Semigroup hiding (Dual)
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Data.Traversable
import Numeric.Algebra
import Numeric.Algebra.Distinguished.Class
import Numeric.Algebra.Dual.Class
import Prelude hiding ((-),(+),(*),negate,subtract, fromInteger,recip)

-- | dual number basis, D^2 = 0. D /= 0.
data DualBasis = E | D deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded,Data,Typeable)

instance Distinguished DualBasis where
  e = E

instance Infinitesimal DualBasis where
  d = D

instance Rig r => Distinguished (DualBasis -> r) where
  e E = one
  e _ = zero

instance Rig r => Infinitesimal (DualBasis -> r) where
  d D = one
  d _       = zero 


instance Representable Dual where
  type Rep Dual = DualBasis
  tabulate f = Dual (f E) (f D)
  index (Dual a _ ) E = a
  index (Dual _ b ) D = b

instance MonadReader DualBasis Dual where
  ask = askRep
  local = localRep

instance Rng k => CombinatorialFreeAlgebra k DualBasis where
  mult f = f' where
    fe = f E E
    fd = f E D + f D E
    f' E = fe
    f' D = fd

instance Rng k => UnitalCombinatorialFreeAlgebra k DualBasis where
  unit x E = x
  unit _ _ = zero

-- the trivial coalgebra
instance Rng k => CombinatorialFreeCoalgebra k DualBasis where
  comult f E E = f E
  comult f D D = f D
  comult _ _ _ = zero

instance Rng k => CounitalCombinatorialFreeCoalgebra k DualBasis where
  counit f = f E + f D

instance Rng k => CombinatorialFreeBialgebra k DualBasis 

instance (InvolutiveSemiring k, Rng k) => InvolutiveCombinatorialFreeAlgebra k DualBasis where
  inv f = f' where
    afe = adjoint (f E)
    nfd = negate (f D)
    f' E = afe
    f' D = nfd

instance (InvolutiveSemiring k, Rng k) => InvolutiveCombinatorialFreeCoalgebra k DualBasis where
  coinv = inv

instance (InvolutiveSemiring k, Rng k) => HopfCombinatorialFreeAlgebra k DualBasis where
  antipode = inv
