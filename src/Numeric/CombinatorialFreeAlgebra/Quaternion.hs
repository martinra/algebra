{-# LANGUAGE FlexibleInstances
           , MultiParamTypeClasses
           , TypeFamilies
           , UndecidableInstances
           , DeriveDataTypeable
           , TypeOperators #-}
module Numeric.CombinatorialFreeAlgebra.Quaternion 
  ( QuaternionBasis(..)
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

instance Distinguished QuaternionBasis where
  e = E

instance Complicated QuaternionBasis where
  i = I

instance Hamiltonian QuaternionBasis where
  j = J
  k = K

instance Rig r => Distinguished (QuaternionBasis -> r) where
  e E = one 
  e _ = zero

instance Rig r => Complicated (QuaternionBasis -> r) where
  i I = one
  i _ = zero
  
instance Rig r => Hamiltonian (QuaternionBasis -> r) where
  j J = one
  j _ = zero

  k K = one
  k _ = zero

-- quaternion basis
data QuaternionBasis = E | I | J | K deriving (Eq,Ord,Enum,Read,Show,Bounded,Ix,Data,Typeable)

instance Representable Quaternion where
  type Rep Quaternion = QuaternionBasis
  tabulate f = Quaternion (f E) (f I) (f J) (f K)
  index (Quaternion a _ _ _) E = a
  index (Quaternion _ b _ _) I = b
  index (Quaternion _ _ c _) J = c
  index (Quaternion _ _ _ d) K = d

instance MonadReader QuaternionBasis Quaternion where
  ask = askRep
  local = localRep

-- | the quaternion algebra
instance (TriviallyInvolutive r, Rng r) => CombinatorialFreeAlgebra r QuaternionBasis where
  mult f = f' where
    fe = f E E - (f I I + f J J + f K K)
    fi = f E I + f I E + f J K - f K J
    fj = f E J + f J E + f K I - f I K
    fk = f E K + f K E + f I J - f J I
    f' E = fe
    f' I = fi
    f' J = fj
    f' K = fk
             
instance (TriviallyInvolutive r, Rng r) => UnitalCombinatorialFreeAlgebra r QuaternionBasis where
  unit x E = x 
  unit _ _ = zero

-- | the trivial diagonal coalgebra
instance (TriviallyInvolutive r, Rng r) => CombinatorialFreeCoalgebra r QuaternionBasis where
  comult f = f' where
    fe = f E
    fi = f I
    fj = f J
    fk = f K
    f' E E = fe
    f' I I = fi
    f' J J = fj
    f' K K = fk
    f' _ _ = zero

instance (TriviallyInvolutive r, Rng r) => CounitalCombinatorialFreeCoalgebra r QuaternionBasis where
  counit f = f E + f I + f J + f K

{-
-- dual quaternion comultiplication
instance (TriviallyInvolutive r, Rng r) => CombinatorialFreeCoalgebra r QuaternionBasis where
  comult f = f' where
    fe = f E
    fi = f I
    fj = f J
    fk = f K
    fe' = negate fe
    fi' = negate fi
    fj' = negate fj
    fk' = negate fk
    f' E E = fe
    f' E I = fi
    f' E J = fj
    f' E K = fk
    f' I E = fi
    f' I I = fe'
    f' I J = fk
    f' I K = fj'
    f' J E = fj
    f' J I = fk'
    f' J J = fe'
    f' J K = fi
    f' K E = fk
    f' K I = fj
    f' K J = fi'
    f' K K = fe'

instance (TriviallyInvolutive r, Rng r) => CounitalCombinatorialFreeCoalgebra r QuaternionBasis where
  counit f = f E
-}

instance (TriviallyInvolutive r, Rng r)  => CombinatorialFreeBialgebra r QuaternionBasis 

instance (TriviallyInvolutive r, InvolutiveSemiring r, Rng r)  => InvolutiveCombinatorialFreeAlgebra r QuaternionBasis where
  inv f E = f E
  inv f b = negate (f b)

instance (TriviallyInvolutive r, InvolutiveSemiring r, Rng r) => InvolutiveCombinatorialFreeCoalgebra r QuaternionBasis where
  coinv = inv

instance (TriviallyInvolutive r, InvolutiveSemiring r, Rng r) => HopfCombinatorialFreeAlgebra r QuaternionBasis where
  antipode = inv

-- | Cayley-Dickson quaternion isomorphism (one way)
complicate :: Complicated c => QuaternionBasis -> (c,c)
complicate E = (e, e)
complicate I = (i, e) 
complicate J = (e, i)
complicate K = (i, i)

scalarPart :: (Representable f, Rep f ~ QuaternionBasis) => f r -> r
scalarPart f = index f E

vectorPart :: (Representable f, Rep f ~ QuaternionBasis) => f r -> (r,r,r)
vectorPart f = (index f I, index f J, index f K)
