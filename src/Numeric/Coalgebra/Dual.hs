{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, UndecidableInstances, DeriveDataTypeable #-}
module Numeric.Coalgebra.Dual
  ( Distinguished(..)
  , Infinitesimal(..)
  , DualBasis'(..)
  , Dual'(..)
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
import Numeric.Algebra.Distinguished.Class
import Numeric.Algebra.Dual.Class
import Prelude hiding ((-),(+),(*),negate,subtract, fromInteger,recip)

-- | dual number basis, D^2 = 0. D /= 0.
data DualBasis' = E | D deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded,Data,Typeable)
data Dual' a = Dual' a a deriving (Eq,Show,Read,Data,Typeable)

instance Distinguished DualBasis' where
  e = E

instance Infinitesimal DualBasis' where
  d = D

instance Rig r => Distinguished (Dual' r) where
  e = Dual' one zero

instance Rig r => Infinitesimal (Dual' r) where
  d = Dual' zero one
  
instance Rig r => Distinguished (DualBasis' -> r) where
  e E = one
  e _ = zero

instance Rig r => Infinitesimal (DualBasis' -> r) where
  d D = one
  d _       = zero 

instance Representable Dual' where
  type Rep Dual' = DualBasis'
  tabulate f = Dual' (f E) (f D)
  index (Dual' a _ ) E = a
  index (Dual' _ b ) D = b

instance Distributive Dual' where
  distribute = distributeRep 

instance Functor Dual' where
  fmap f (Dual' a b) = Dual' (f a) (f b)

instance Apply Dual' where
  (<.>) = apRep

instance Applicative Dual' where
  pure = pureRep
  (<*>) = apRep 

instance Bind Dual' where
  (>>-) = bindRep

instance Monad Dual' where
  return = pureRep
  (>>=) = bindRep

instance MonadReader DualBasis' Dual' where
  ask = askRep
  local = localRep

instance Foldable Dual' where
  foldMap f (Dual' a b) = f a `mappend` f b

instance Traversable Dual' where
  traverse f (Dual' a b) = Dual' <$> f a <*> f b

instance Foldable1 Dual' where
  foldMap1 f (Dual' a b) = f a <> f b

instance Traversable1 Dual' where
  traverse1 f (Dual' a b) = Dual' <$> f a <.> f b

instance Additive r => Additive (Dual' r) where
  (+) = addRep 
  sinnum1p = sinnum1pRep

instance LeftModule r s => LeftModule r (Dual' s) where
  r .* Dual' a b = Dual' (r .* a) (r .* b)

instance RightModule r s => RightModule r (Dual' s) where
  Dual' a b *. r = Dual' (a *. r) (b *. r)

instance Monoidal r => Monoidal (Dual' r) where
  zero = zeroRep
  sinnum = sinnumRep

instance Group r => Group (Dual' r) where
  (-) = minusRep
  negate = negateRep
  subtract = subtractRep
  times = timesRep

instance Abelian r => Abelian (Dual' r)

instance Idempotent r => Idempotent (Dual' r)

instance Partitionable r => Partitionable (Dual' r) where
  partitionWith f (Dual' a b) = id =<<
    partitionWith (\a1 a2 -> 
    partitionWith (\b1 b2 -> f (Dual' a1 b1) (Dual' a2 b2)) b) a

instance Semiring k => CombinatorialFreeAlgebra k DualBasis' where
  mult f = f' where
    fe = f E E
    fd = f D D
    f' E = fe
    f' D = fd

instance Semiring k => UnitalCombinatorialFreeAlgebra k DualBasis' where
  unit = const

-- the trivial coalgebra
instance Rng k => CombinatorialFreeCoalgebra k DualBasis' where
  comult f = f' where
     fe = f E
     fd = f D
     f' E E = fe
     f' E D = fd
     f' D E = fd
     f' D D = zero

instance Rng k => CounitalCombinatorialFreeCoalgebra k DualBasis' where
  counit f = f E

instance Rng k => CombinatorialFreeBialgebra k DualBasis' 

instance (InvolutiveSemiring k, Rng k) => InvolutiveCombinatorialFreeAlgebra k DualBasis' where
  inv f = f' where
    afe = adjoint (f E)
    nfd = negate (f D)
    f' E = afe
    f' D = nfd

instance (InvolutiveSemiring k, Rng k) => InvolutiveCombinatorialFreeCoalgebra k DualBasis' where
  coinv = inv

instance (InvolutiveSemiring k, Rng k) => HopfCombinatorialFreeAlgebra k DualBasis' where
  antipode = inv

instance (Commutative r, Rng r) => Multiplicative (Dual' r) where
  (*) = mulRep

instance (TriviallyInvolutive r, Rng r) => Commutative (Dual' r)

instance (Commutative r, Rng r) => Semiring (Dual' r)

instance (Commutative r, Ring r) => Unital (Dual' r) where
  one = oneRep

instance (Commutative r, Ring r) => Rig (Dual' r) where
  fromNatural n = Dual' (fromNatural n) zero

instance (Commutative r, Ring r) => Ring (Dual' r) where
  fromInteger n = Dual' (fromInteger n) zero

instance (Commutative r, Rng r) => LeftModule (Dual' r) (Dual' r) where (.*) = (*)
instance (Commutative r, Rng r) => RightModule (Dual' r) (Dual' r) where (*.) = (*)

instance (Commutative r, Rng r, InvolutiveSemiring r) => InvolutiveMultiplication (Dual' r) where
  adjoint (Dual' a b) = Dual' (adjoint a) (negate b)

instance (Commutative r, Rng r, InvolutiveSemiring r) => InvolutiveSemiring (Dual' r)

instance (Commutative r, Rng r, InvolutiveSemiring r) => Quadrance r (Dual' r) where
  quadrance n = case adjoint n * n of
    Dual' a _ -> a

instance (Commutative r, InvolutiveSemiring r, DivisionRing r) => Division (Dual' r) where
  recip q@(Dual' a b) = Dual' (qq \\ a) (qq \\ b)
    where qq = quadrance q
