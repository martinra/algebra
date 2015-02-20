{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, TypeFamilies, UndecidableInstances, DeriveDataTypeable #-}
module Numeric.Algebra.Hyperbolic
  ( Hyperbolic(..)
  , Hyper'(..)
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
data Hyper' a = Hyper' a a deriving (Eq,Show,Read,Data,Typeable)

instance Rig r => Hyperbolic (Hyper' r) where
  cosh = Hyper' one zero
  sinh = Hyper' zero one
  
instance Distributive Hyper' where
  distribute = distributeRep 

instance Functor Hyper' where
  fmap f (Hyper' a b) = Hyper' (f a) (f b)

instance Apply Hyper' where
  (<.>) = apRep

instance Applicative Hyper' where
  pure = pureRep
  (<*>) = apRep 

instance Bind Hyper' where
  (>>-) = bindRep

instance Monad Hyper' where
  return = pureRep
  (>>=) = bindRep

instance Foldable Hyper' where
  foldMap f (Hyper' a b) = f a `mappend` f b

instance Traversable Hyper' where
  traverse f (Hyper' a b) = Hyper' <$> f a <*> f b

instance Foldable1 Hyper' where
  foldMap1 f (Hyper' a b) = f a <> f b

instance Traversable1 Hyper' where
  traverse1 f (Hyper' a b) = Hyper' <$> f a <.> f b

instance Additive r => Additive (Hyper' r) where
  (+) = addRep 
  sinnum1p = sinnum1pRep

instance LeftModule r s => LeftModule r (Hyper' s) where
  r .* Hyper' a b = Hyper' (r .* a) (r .* b)

instance RightModule r s => RightModule r (Hyper' s) where
  Hyper' a b *. r = Hyper' (a *. r) (b *. r)

instance Monoidal r => Monoidal (Hyper' r) where
  zero = zeroRep
  sinnum = sinnumRep

instance Group r => Group (Hyper' r) where
  (-) = minusRep
  negate = negateRep
  subtract = subtractRep
  times = timesRep

instance Abelian r => Abelian (Hyper' r)

instance Idempotent r => Idempotent (Hyper' r)

instance Partitionable r => Partitionable (Hyper' r) where
  partitionWith f (Hyper' a b) = id =<<
    partitionWith (\a1 a2 -> 
    partitionWith (\b1 b2 -> f (Hyper' a1 b1) (Hyper' a2 b2)) b) a

instance (Commutative k, Semiring k) => Multiplicative (Hyper' k) where
  (*) = mulRep

instance (Commutative k, Semiring k) => Commutative (Hyper' k)

instance (Commutative k, Semiring k) => Semiring (Hyper' k)

instance (Commutative k, Rig k) => Unital (Hyper' k) where
  one = Hyper' one zero

instance (Commutative r, Rig r) => Rig (Hyper' r) where
  fromNatural n = Hyper' (fromNatural n) zero

instance (Commutative r, Ring r) => Ring (Hyper' r) where
  fromInteger n = Hyper' (fromInteger n) zero

instance (Commutative r, Semiring r) => LeftModule (Hyper' r) (Hyper' r) where (.*) = (*)
instance (Commutative r, Semiring r) => RightModule (Hyper' r) (Hyper' r) where (*.) = (*)

instance (Commutative r, InvolutiveSemiring r, Rng r) => InvolutiveMultiplication (Hyper' r) where
  adjoint (Hyper' a b) = Hyper' (adjoint a) (negate b)

instance (Commutative r, InvolutiveSemiring r, Rng r) => InvolutiveSemiring (Hyper' r)

instance (Commutative r, InvolutiveSemiring r, Rng r) => Quadrance r (Hyper' r) where
  quadrance n = case adjoint n * n of
    Hyper' a _ -> a

instance (Commutative r, InvolutiveSemiring r, DivisionRing r) => Division (Hyper' r) where
  recip q@(Hyper' a b) = Hyper' (qq \\ a) (qq \\ b)
    where qq = quadrance q

