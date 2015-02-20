{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
module Numeric.Covector
  ( Covector(..)
  -- * Covectors as linear functionals
  , counitM
  , unitM
  , comultM
  , multM
  , invM
  , coinvM
  , antipodeM
  , convolveM
  ) where

import Numeric.Additive.Class
import Numeric.Additive.Group
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Algebra.Idempotent
import Numeric.Algebra.Commutative
import Numeric.CombinatorialFreeAlgebra.Class
import Numeric.CombinatorialFreeAlgebra.Unital
import Numeric.CombinatorialFreeAlgebra.Idempotent
import Numeric.CombinatorialFreeAlgebra.Involutive
import Numeric.CombinatorialFreeAlgebra.Hopf
import Numeric.Rig.Class
import Numeric.Ring.Class
import Control.Applicative
import Control.Monad
import Data.Functor.Plus hiding (zero)
import qualified Data.Functor.Plus as Plus
import Data.Functor.Bind
import Prelude hiding ((+),(-),negate,subtract,replicate,(*))

-- | Linear functionals from elements of an (infinite) free module to a scalar

-- f $* (x + y) = (f $* x) + (f $* y)
-- f $* (a .* x) = a * (f $* x)

infixr 0 $*
newtype Covector r a = Covector { ($*) :: (a -> r) -> r }

instance Functor (Covector r) where
  fmap f m = Covector $ \k -> m $* k . f

instance Apply (Covector r) where
  mf <.> ma = Covector $ \k -> mf $* \f -> ma $* k . f

instance Applicative (Covector r) where
  pure a = Covector $ \k -> k a
  mf <*> ma = Covector $ \k -> mf $* \f -> ma $* k . f

instance Bind (Covector r) where
  m >>- f = Covector $ \k -> m $* \a -> f a $* k
  
instance Monad (Covector r) where
  return a = Covector $ \k -> k a
  m >>= f = Covector $ \k -> m $* \a -> f a $* k

instance Additive r => Alt (Covector r) where
  Covector m <!> Covector n = Covector $ m + n

instance Monoidal r => Plus (Covector r) where
  zero = Covector zero 

instance Monoidal r => Alternative (Covector r) where
  Covector m <|> Covector n = Covector $ m + n
  empty = Covector zero

instance Monoidal r => MonadPlus (Covector r) where
  Covector m `mplus` Covector n = Covector $ m + n
  mzero = Covector zero

instance Additive r => Additive (Covector r a) where
  Covector m + Covector n = Covector $ m + n
  sinnum1p n (Covector m) = Covector $ sinnum1p n m

instance CombinatorialFreeCoalgebra r m => Multiplicative (Covector r m) where
  Covector f * Covector g = Covector $ \k -> f (\m -> g (comult k m))

instance (Commutative m, CombinatorialFreeCoalgebra r m) => Commutative (Covector r m)

instance CombinatorialFreeCoalgebra r m => Semiring (Covector r m)

instance CounitalCombinatorialFreeCoalgebra r m => Unital (Covector r m) where
  one = Covector counit

instance (Rig r, CounitalCombinatorialFreeCoalgebra r m) => Rig (Covector r m)

instance (Ring r, CounitalCombinatorialFreeCoalgebra r m) => Ring (Covector r m)

instance Idempotent r => Idempotent (Covector r a)

instance (Idempotent r, IdempotentCombinatorialFreeCoalgebra r a) => Band (Covector r a)

multM :: CombinatorialFreeCoalgebra r c => c -> c -> Covector r c
multM a b = Covector $ \k -> comult k a b

unitM :: CounitalCombinatorialFreeCoalgebra r c => Covector r c
unitM = Covector counit

comultM :: CombinatorialFreeAlgebra r a => a -> Covector r (a,a)
comultM c = Covector $ \k -> mult (curry k) c 

counitM :: UnitalCombinatorialFreeAlgebra r a => a -> Covector r ()
counitM a = Covector $ \k -> unit (k ()) a

convolveM :: (CombinatorialFreeAlgebra r c, CombinatorialFreeCoalgebra r a) => (c -> Covector r a) -> (c -> Covector r a) -> c -> Covector r a
convolveM f g c = do
   (c1,c2) <- comultM c
   a1 <- f c1
   a2 <- g c2
   multM a1 a2

invM :: InvolutiveCombinatorialFreeAlgebra r h => h -> Covector r h
invM = Covector . flip inv

coinvM :: InvolutiveCombinatorialFreeCoalgebra r h => h -> Covector r h
coinvM = Covector . flip coinv

-- | convolveM antipodeM return = convolveM return antipodeM = comultM >=> uncurry joinM
antipodeM :: HopfCombinatorialFreeAlgebra r h => h -> Covector r h
antipodeM = Covector . flip antipode

-- TODO: we can also build up the augmentation ideal

instance Monoidal s => Monoidal (Covector s a) where
  zero = Covector zero
  sinnum n (Covector m) = Covector (sinnum n m)

instance Abelian s => Abelian (Covector s a)

instance Group s => Group (Covector s a) where
  Covector m - Covector n = Covector $ m - n
  negate (Covector m) = Covector $ negate m
  subtract (Covector m) (Covector n) = Covector $ subtract m n
  times n (Covector m) = Covector $ times n m

instance CombinatorialFreeCoalgebra r m => LeftModule (Covector r m) (Covector r m) where
  (.*) = (*)

instance LeftModule r s => LeftModule r (Covector s m) where
  s .* m = Covector $ \k -> s .* (m $* k)

instance CombinatorialFreeCoalgebra r m => RightModule (Covector r m) (Covector r m) where
  (*.) = (*)

instance RightModule r s => RightModule r (Covector s m) where
  m *. s = Covector $ \k -> (m $* k) *. s
