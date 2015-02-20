{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , UndecidableInstances
           , DeriveDataTypeable
           , TypeOperators #-}
module Numeric.Algebra.Quaternion 
  ( Distinguished(..)
  , Complicated(..)
  , Hamiltonian(..)
  , Quaternion(..)
  , complicate
  , vectorPart
  , scalarPart
  ) where

import Control.Applicative
import Control.Monad.Reader.Class
import Data.Ix hiding (index)
import Data.Data
import Data.Distributive
import Data.Functor.Bind
import Data.Functor.Rep
import Data.Foldable
import Data.Traversable
import Data.Semigroup
import Data.Semigroup.Traversable
import Data.Semigroup.Foldable
import Numeric.Algebra
import Numeric.Algebra.Distinguished.Class
import Numeric.Algebra.Complex.Class
import Numeric.Algebra.Quaternion.Class
import Prelude hiding ((-),(+),(*),negate,subtract, fromInteger)

instance Rig r => Distinguished (Quaternion r) where
  e = Quaternion one zero zero zero

instance Rig r => Complicated (Quaternion r) where
  i = Quaternion zero one zero zero

instance Rig r => Hamiltonian (Quaternion r) where
  j = Quaternion zero zero one zero
  k = Quaternion one zero zero one 

data Quaternion a = Quaternion a a a a deriving (Eq,Show,Read,Data,Typeable)

instance Distributive Quaternion where
  distribute = distributeRep 

instance Functor Quaternion where
  fmap = fmapRep

instance Apply Quaternion where
  (<.>) = apRep

instance Applicative Quaternion where
  pure = pureRep
  (<*>) = apRep 

instance Bind Quaternion where
  (>>-) = bindRep

instance Monad Quaternion where
  return = pureRep
  (>>=) = bindRep

instance Foldable Quaternion where
  foldMap f (Quaternion a b c d) = 
    f a `mappend` f b `mappend` f c `mappend` f d

instance Traversable Quaternion where
  traverse f (Quaternion a b c d) = 
    Quaternion <$> f a <*> f b <*> f c <*> f d

instance Foldable1 Quaternion where
  foldMap1 f (Quaternion a b c d) = 
    f a <> f b <> f c <> f d

instance Traversable1 Quaternion where
  traverse1 f (Quaternion a b c d) = 
    Quaternion <$> f a <.> f b <.> f c <.> f d

instance Additive r => Additive (Quaternion r) where
  (+) = addRep 
  sinnum1p = sinnum1pRep

instance LeftModule r s => LeftModule r (Quaternion s) where
  r .* Quaternion a b c d =
    Quaternion (r .* a) (r .* b) (r .* c) (r .* d)

instance RightModule r s => RightModule r (Quaternion s) where
  Quaternion a b c d *. r =
    Quaternion (a *. r) (b *. r) (c *. r) (d *. r)

instance Monoidal r => Monoidal (Quaternion r) where
  zero = zeroRep
  sinnum = sinnumRep

instance Group r => Group (Quaternion r) where
  (-) = minusRep
  negate = negateRep
  subtract = subtractRep
  times = timesRep

instance Abelian r => Abelian (Quaternion r)

instance Idempotent r => Idempotent (Quaternion r)

instance Partitionable r => Partitionable (Quaternion r) where
  partitionWith f (Quaternion a b c d) = id =<<
    partitionWith (\a1 a2 -> id =<< 
    partitionWith (\b1 b2 -> id =<< 
    partitionWith (\c1 c2 -> 
    partitionWith (\d1 d2 -> f (Quaternion a1 b1 c1 d1) 
                               (Quaternion a2 b2 c2 d2)
                  ) d) c) b) a

instance (TriviallyInvolutive r, Rng r) => Multiplicative (Quaternion r) where
  (*) = mulRep

instance (TriviallyInvolutive r, Rng r) => Semiring (Quaternion r)

instance (TriviallyInvolutive r, Ring r) => Unital (Quaternion r) where
  one = oneRep

instance (TriviallyInvolutive r, Ring r) => Rig (Quaternion r) where
  fromNatural n = Quaternion (fromNatural n) zero zero zero

instance (TriviallyInvolutive r, Ring r) => Ring (Quaternion r) where
  fromInteger n = Quaternion (fromInteger n) zero zero zero

instance ( TriviallyInvolutive r, Rng r) => LeftModule (Quaternion r) (Quaternion r) where 
  (.*) = (*)
instance (TriviallyInvolutive r, Rng r) => RightModule (Quaternion r) (Quaternion r) where 
  (*.) = (*)

instance (TriviallyInvolutive r, Rng r) => InvolutiveMultiplication (Quaternion r) where
  -- without trivial involution, multiplication fails associativity, and we'd need to 
  -- support weaker multiplicative properties like Alternative and PowerAssociative
  adjoint (Quaternion a b c d) = Quaternion a (negate b) (negate c) (negate d)

instance (TriviallyInvolutive r, Rng r) => Quadrance r (Quaternion r) where
  quadrance n = scalarPart (adjoint n * n)

instance (TriviallyInvolutive r, Ring r, Division r) => Division (Quaternion r) where
  recip q@(Quaternion a b c d) = Quaternion (qq \\ a) (qq \\ b) (qq \\ c) (qq \\ d)
    where qq = quadrance q
