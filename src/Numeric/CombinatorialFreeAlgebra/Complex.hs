{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , TypeFamilies
           , UndecidableInstances
           , DeriveDataTypeable
           , TypeOperators #-}
module Numeric.CombinatorialFreeAlgebra.Complex
  ( Distinguished(..)
  , Complicated(..)
  , ComplexBasis(..)
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
data ComplexBasis = E | I deriving (Eq,Ord,Show,Read,Enum,Ix,Bounded,Data,Typeable)

realPart :: (Representable f, Rep f ~ ComplexBasis) => f a -> a
realPart f = index f E 

imagPart :: (Representable f, Rep f ~ ComplexBasis) => f a -> a
imagPart f = index f I

instance Distinguished ComplexBasis where
  e = E
  
instance Complicated ComplexBasis where
  i = I

instance Rig r => Distinguished (ComplexBasis -> r) where
  e E = one
  e _ = zero
  
instance Rig r => Complicated (ComplexBasis -> r) where
  i I = one
  i _ = zero 

instance Representable Complex where
  type Rep Complex = ComplexBasis
  tabulate f = Complex (f E) (f I)
  index (Complex a _ ) E = a
  index (Complex _ b ) I = b

instance MonadReader ComplexBasis Complex where
  ask = askRep
  local = localRep

instance Rng k => CombinatorialFreeAlgebra k ComplexBasis where
  mult f = f' where
    fe = f E E - f I I
    fi = f E I + f I E
    f' E = fe
    f' I = fi

instance Rng k => UnitalCombinatorialFreeAlgebra k ComplexBasis where
  unit x E = x
  unit _ _ = zero

-- the trivial coalgebra
instance Rng k => CombinatorialFreeCoalgebra k ComplexBasis where
  comult f E E = f E
  comult f I I = f I
  comult _ _ _ = zero

instance Rng k => CounitalCombinatorialFreeCoalgebra k ComplexBasis where
  counit f = f E + f I

instance Rng k => CombinatorialFreeBialgebra k ComplexBasis 

instance (InvolutiveSemiring k, Rng k) => InvolutiveCombinatorialFreeAlgebra k ComplexBasis where
  inv f = f' where
    afe = adjoint (f E)
    nfi = negate (f I)
    f' E = afe
    f' I = nfi

instance (InvolutiveSemiring k, Rng k) => InvolutiveCombinatorialFreeCoalgebra k ComplexBasis where
  coinv = inv

instance (InvolutiveSemiring k, Rng k) => HopfCombinatorialFreeAlgebra k ComplexBasis where
  antipode = inv

-- | half of the Cayley-Dickson quaternion isomorphism 
uncomplicate :: Hamiltonian q => ComplexBasis -> ComplexBasis -> q
uncomplicate E E = e
uncomplicate I E = i
uncomplicate E I = j
uncomplicate I I = k

