{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Algebra.Unital
  ( 
  -- * Unital Multiplication (Multiplicative monoid)
    Unital(..)
  , product
  -- * Unital Associative CombinatorialFreeAlgebra 
  , UnitalCombinatorialFreeAlgebra(..)
  -- * Unital Coassociative CombinatorialFreeCoalgebra
  , CounitalCombinatorialFreeCoalgebra(..)
  -- * CombinatorialFreeBialgebra
  , CombinatorialFreeBialgebra
  ) where

import Numeric.Algebra.Class
import Numeric.Natural
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Foldable hiding (product)
import Data.Int
import Data.Word
import Prelude hiding ((*), foldr, product)

infixr 8 `pow`

class Multiplicative r => Unital r where
  one :: r
  pow :: r -> Natural -> r
  pow _ 0 = one
  pow x0 y0 = f x0 y0 where
    f x y 
      | even y = f (x * x) (y `quot` 2)
      | y == 1 = x
      | otherwise = g (x * x) ((y - 1) `quot` 2) x
    g x y z 
      | even y = g (x * x) (y `quot` 2) z
      | y == 1 = x * z
      | otherwise = g (x * x) ((y - 1) `quot` 2) (x * z)
  productWith :: Foldable f => (a -> r) -> f a -> r
  productWith f = foldl' (\b a -> b * f a) one

product :: (Foldable f, Unital r) => f r -> r
product = productWith id

instance Unital Bool where one = True
instance Unital Integer where one = 1
instance Unital Int where one = 1
instance Unital Int8 where one = 1
instance Unital Int16 where one = 1
instance Unital Int32 where one = 1
instance Unital Int64 where one = 1
instance Unital Natural where one = 1
instance Unital Word where one = 1
instance Unital Word8 where one = 1
instance Unital Word16 where one = 1
instance Unital Word32 where one = 1
instance Unital Word64 where one = 1
instance Unital () where one = ()
instance (Unital a, Unital b) => Unital (a,b) where
  one = (one,one)

instance (Unital a, Unital b, Unital c) => Unital (a,b,c) where
  one = (one,one,one)

instance (Unital a, Unital b, Unital c, Unital d) => Unital (a,b,c,d) where
  one = (one,one,one,one)

instance (Unital a, Unital b, Unital c, Unital d, Unital e) => Unital (a,b,c,d,e) where
  one = (one,one,one,one,one)

-- | An associative unital algebra over a semiring, built using a free module
class CombinatorialFreeAlgebra r a => UnitalCombinatorialFreeAlgebra r a where
  unit :: r -> a -> r

instance (Unital r, UnitalCombinatorialFreeAlgebra r a) => Unital (a -> r) where
  one = unit one

instance Semiring r => UnitalCombinatorialFreeAlgebra r () where
  unit r () = r

-- incoherent
-- instance UnitalCombinatorialFreeAlgebra () a where unit _ _ = ()
-- instance (UnitalCombinatorialFreeAlgebra r a, UnitalCombinatorialFreeAlgebra r b) => UnitalCombinatorialFreeAlgebra (a -> r) b where unit f b a = unit (f a) b

instance (UnitalCombinatorialFreeAlgebra r a, UnitalCombinatorialFreeAlgebra r b) => UnitalCombinatorialFreeAlgebra r (a,b) where
  unit r (a,b) = unit r a * unit r b

instance (UnitalCombinatorialFreeAlgebra r a, UnitalCombinatorialFreeAlgebra r b, UnitalCombinatorialFreeAlgebra r c) => UnitalCombinatorialFreeAlgebra r (a,b,c) where
  unit r (a,b,c) = unit r a * unit r b * unit r c

instance (UnitalCombinatorialFreeAlgebra r a, UnitalCombinatorialFreeAlgebra r b, UnitalCombinatorialFreeAlgebra r c, UnitalCombinatorialFreeAlgebra r d) => UnitalCombinatorialFreeAlgebra r (a,b,c,d) where
  unit r (a,b,c,d) = unit r a * unit r b * unit r c * unit r d

instance (UnitalCombinatorialFreeAlgebra r a, UnitalCombinatorialFreeAlgebra r b, UnitalCombinatorialFreeAlgebra r c, UnitalCombinatorialFreeAlgebra r d, UnitalCombinatorialFreeAlgebra r e) => UnitalCombinatorialFreeAlgebra r (a,b,c,d,e) where
  unit r (a,b,c,d,e) = unit r a * unit r b * unit r c * unit r d * unit r e

instance (Monoidal r, Semiring r) => UnitalCombinatorialFreeAlgebra r [a] where
  unit r [] = r
  unit _ _ = zero

instance (Monoidal r, Semiring r) => UnitalCombinatorialFreeAlgebra r (Seq a) where
  unit r a | Seq.null a = r
           | otherwise = zero

-- A coassociative counital coalgebra over a semiring, where the module is free
class CombinatorialFreeCoalgebra r c => CounitalCombinatorialFreeCoalgebra r c where
  counit :: (c -> r) -> r

instance (Unital r, UnitalCombinatorialFreeAlgebra r m) => CounitalCombinatorialFreeCoalgebra r (m -> r) where
  counit k = k one

-- incoherent
-- instance (UnitalCombinatorialFreeAlgebra r a, CounitalCombinatorialFreeCoalgebra r c) => CounitalCombinatorialFreeCoalgebra (a -> r) c where counit k a = counit (`k` a)
-- instance CounitalCombinatorialFreeCoalgebra () a where counit _ = ()

instance Semiring r => CounitalCombinatorialFreeCoalgebra r () where
  counit f = f ()

instance (CounitalCombinatorialFreeCoalgebra r a, CounitalCombinatorialFreeCoalgebra r b) => CounitalCombinatorialFreeCoalgebra r (a, b) where
  counit k = counit $ \a -> counit $ \b -> k (a,b)

instance (CounitalCombinatorialFreeCoalgebra r a, CounitalCombinatorialFreeCoalgebra r b, CounitalCombinatorialFreeCoalgebra r c) => CounitalCombinatorialFreeCoalgebra r (a, b, c) where
  counit k = counit $ \a -> counit $ \b -> counit $ \c -> k (a,b,c)

instance (CounitalCombinatorialFreeCoalgebra r a, CounitalCombinatorialFreeCoalgebra r b, CounitalCombinatorialFreeCoalgebra r c, CounitalCombinatorialFreeCoalgebra r d) => CounitalCombinatorialFreeCoalgebra r (a, b, c, d) where
  counit k = counit $ \a -> counit $ \b -> counit $ \c -> counit $ \d -> k (a,b,c,d)

instance (CounitalCombinatorialFreeCoalgebra r a, CounitalCombinatorialFreeCoalgebra r b, CounitalCombinatorialFreeCoalgebra r c, CounitalCombinatorialFreeCoalgebra r d, CounitalCombinatorialFreeCoalgebra r e) => CounitalCombinatorialFreeCoalgebra r (a, b, c, d, e) where
  counit k = counit $ \a -> counit $ \b -> counit $ \c -> counit $ \d -> counit $ \e -> k (a,b,c,d,e)

instance Semiring r => CounitalCombinatorialFreeCoalgebra r [a] where
  counit k = k []

instance Semiring r => CounitalCombinatorialFreeCoalgebra r (Seq a) where
  counit k = k (Seq.empty)

-- | A bialgebra is both a unital algebra and counital coalgebra 
-- where the `mult` and `unit` are compatible in some sense with 
-- the `comult` and `counit`. That is to say that 
-- 'mult' and 'unit' are a coalgebra homomorphisms or (equivalently) that 
-- 'comult' and 'counit' are an algebra homomorphisms.

class (UnitalCombinatorialFreeAlgebra r a, CounitalCombinatorialFreeCoalgebra r a) => CombinatorialFreeBialgebra r a

-- TODO
-- instance (Unital r, CombinatorialFreeBialgebra r m) => CombinatorialFreeBialgebra r (m -> r)
-- instance CombinatorialFreeBialgebra () c
-- instance (UnitalCombinatorialFreeAlgebra r b, CombinatorialFreeBialgebra r c) => CombinatorialFreeBialgebra (b -> r) c

instance Semiring r => CombinatorialFreeBialgebra r ()
instance (CombinatorialFreeBialgebra r a, CombinatorialFreeBialgebra r b) => CombinatorialFreeBialgebra r (a, b)
instance (CombinatorialFreeBialgebra r a, CombinatorialFreeBialgebra r b, CombinatorialFreeBialgebra r c) => CombinatorialFreeBialgebra r (a, b, c)
instance (CombinatorialFreeBialgebra r a, CombinatorialFreeBialgebra r b, CombinatorialFreeBialgebra r c, CombinatorialFreeBialgebra r d) => CombinatorialFreeBialgebra r (a, b, c, d)
instance (CombinatorialFreeBialgebra r a, CombinatorialFreeBialgebra r b, CombinatorialFreeBialgebra r c, CombinatorialFreeBialgebra r d, CombinatorialFreeBialgebra r e) => CombinatorialFreeBialgebra r (a, b, c, d, e)

instance (Monoidal r, Semiring r) => CombinatorialFreeBialgebra r [a]
instance (Monoidal r, Semiring r) => CombinatorialFreeBialgebra r (Seq a)
