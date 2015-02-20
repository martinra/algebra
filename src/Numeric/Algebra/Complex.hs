{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , TypeFamilies
           , UndecidableInstances
           , DeriveDataTypeable
           , TypeOperators #-}
module Numeric.Algebra.Complex
  ( Distinguished(..)
  , Complicated(..)
  , Complex(..)
  , realPart
  , imagPart
  , uncomplicate
  ) where

import Control.Applicative
import Control.Monad.Reader.Class
import Data.Data
import Data.Distributive
import Data.Functor.Bind
import Data.Functor.Rep
import Data.Foldable
import Data.Ix hiding (index)
import Data.Semigroup
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Data.Traversable
import Numeric.Algebra
import Numeric.Algebra.Distinguished.Class
import Numeric.Algebra.Complex.Class
import Numeric.Algebra.Quaternion.Class
import Prelude hiding ((-),(+),(*),negate,subtract, fromInteger,recip)

-- complex basis
data Complex a = Complex a a deriving (Eq,Show,Read,Data,Typeable)


instance Rig r => Distinguished (Complex r) where
  e = Complex one zero

instance Rig r => Complicated (Complex r) where
  i = Complex zero one

instance Distributive Complex where
  distribute = distributeRep 

instance Functor Complex where
  fmap f (Complex a b) = Complex (f a) (f b)

instance Apply Complex where
  (<.>) = apRep

instance Applicative Complex where
  pure = pureRep
  (<*>) = apRep 

instance Bind Complex where
  (>>-) = bindRep

instance Monad Complex where
  return = pureRep
  (>>=) = bindRep

instance Foldable Complex where
  foldMap f (Complex a b) = f a `mappend` f b

instance Traversable Complex where
  traverse f (Complex a b) = Complex <$> f a <*> f b

instance Foldable1 Complex where
  foldMap1 f (Complex a b) = f a <> f b

instance Traversable1 Complex where
  traverse1 f (Complex a b) = Complex <$> f a <.> f b

instance Additive r => Additive (Complex r) where
  (+) = addRep 
  sinnum1p = sinnum1pRep

instance LeftModule r s => LeftModule r (Complex s) where
  r .* Complex a b = Complex (r .* a) (r .* b)

instance RightModule r s => RightModule r (Complex s) where
  Complex a b *. r = Complex (a *. r) (b *. r)

instance Monoidal r => Monoidal (Complex r) where
  zero = zeroRep
  sinnum = sinnumRep

instance Group r => Group (Complex r) where
  (-) = minusRep
  negate = negateRep
  subtract = subtractRep
  times = timesRep

instance Abelian r => Abelian (Complex r)

instance Idempotent r => Idempotent (Complex r)

instance Partitionable r => Partitionable (Complex r) where
  partitionWith f (Complex a b) = id =<<
    partitionWith (\a1 a2 -> 
    partitionWith (\b1 b2 -> f (Complex a1 b1) (Complex a2 b2)) b) a

instance (Commutative r, Rng r) => Multiplicative (Complex r) where
  (*) = mulRep

instance (TriviallyInvolutive r, Rng r) => Commutative (Complex r)

instance (Commutative r, Rng r) => Semiring (Complex r)

instance (Commutative r, Ring r) => Unital (Complex r) where
  one = oneRep

instance (Commutative r, Ring r) => Rig (Complex r) where
  fromNatural n = Complex (fromNatural n) zero

instance (Commutative r, Ring r) => Ring (Complex r) where
  fromInteger n = Complex (fromInteger n) zero

instance (Commutative r, Rng r) => LeftModule (Complex r) (Complex r) where (.*) = (*)
instance (Commutative r, Rng r) => RightModule (Complex r) (Complex r) where (*.) = (*)

instance (Commutative r, Rng r, InvolutiveMultiplication r) => InvolutiveMultiplication (Complex r) where
  adjoint (Complex a b) = Complex (adjoint a) (negate b)

instance (Commutative r, Rng r, InvolutiveSemiring r) => InvolutiveSemiring (Complex r)

instance (Commutative r, Rng r, InvolutiveSemiring r) => Quadrance r (Complex r) where
  quadrance n = realPart $ adjoint n * n

instance (Commutative r, InvolutiveSemiring r, DivisionRing r) => Division (Complex r) where
  recip q@(Complex a b) = Complex (qq \\ a) (qq \\ b)
    where qq = quadrance q
