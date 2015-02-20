{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.CombinatorialFreeAlgebra.Unital
  ( -- * Unital Associative CombinatorialFreeAlgebra 
   UnitalCombinatorialFreeAlgebra(..)
  -- * Unital Coassociative CombinatorialFreeCoalgebra
  , CounitalCombinatorialFreeCoalgebra(..)
  -- * CombinatorialFreeBialgebra
  , CombinatorialFreeBialgebra
  ) where

import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.CombinatorialFreeAlgebra.Class
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Prelude hiding ((*), foldr, product)


-- | An associative unital algebra over a semiring, built using a free module
class CombinatorialFreeAlgebra r a => UnitalCombinatorialFreeAlgebra r a where
  unitCF :: r -> a -> r

instance (Unital r, UnitalCombinatorialFreeAlgebra r a) => Unital (a -> r) where
  one = unitCF one

instance Semiring r => UnitalCombinatorialFreeAlgebra r () where
  unitCF r () = r

-- incoherent
-- instance UnitalCombinatorialFreeAlgebra () a where unit _ _ = ()
-- instance (UnitalCombinatorialFreeAlgebra r a, UnitalCombinatorialFreeAlgebra r b) => UnitalCombinatorialFreeAlgebra (a -> r) b where unit f b a = unit (f a) b

instance (UnitalCombinatorialFreeAlgebra r a, UnitalCombinatorialFreeAlgebra r b) => UnitalCombinatorialFreeAlgebra r (a,b) where
  unitCF r (a,b) = unitCF r a * unitCF r b

instance (UnitalCombinatorialFreeAlgebra r a, UnitalCombinatorialFreeAlgebra r b, UnitalCombinatorialFreeAlgebra r c) => UnitalCombinatorialFreeAlgebra r (a,b,c) where
  unitCF r (a,b,c) = unitCF r a * unitCF r b * unitCF r c

instance (UnitalCombinatorialFreeAlgebra r a, UnitalCombinatorialFreeAlgebra r b, UnitalCombinatorialFreeAlgebra r c, UnitalCombinatorialFreeAlgebra r d) => UnitalCombinatorialFreeAlgebra r (a,b,c,d) where
  unitCF r (a,b,c,d) = unitCF r a * unitCF r b * unitCF r c * unitCF r d

instance (UnitalCombinatorialFreeAlgebra r a, UnitalCombinatorialFreeAlgebra r b, UnitalCombinatorialFreeAlgebra r c, UnitalCombinatorialFreeAlgebra r d, UnitalCombinatorialFreeAlgebra r e) => UnitalCombinatorialFreeAlgebra r (a,b,c,d,e) where
  unitCF r (a,b,c,d,e) = unitCF r a * unitCF r b * unitCF r c * unitCF r d * unitCF r e

instance (Monoidal r, Semiring r) => UnitalCombinatorialFreeAlgebra r [a] where
  unitCF r [] = r
  unitCF _ _ = zero

instance (Monoidal r, Semiring r) => UnitalCombinatorialFreeAlgebra r (Seq a) where
  unitCF r a | Seq.null a = r
           | otherwise = zero

-- A coassociative counital coalgebra over a semiring, where the module is free
class CombinatorialFreeCoalgebra r c => CounitalCombinatorialFreeCoalgebra r c where
  counitCF :: (c -> r) -> r

instance (Unital r, UnitalCombinatorialFreeAlgebra r m) => CounitalCombinatorialFreeCoalgebra r (m -> r) where
  counitCF k = k one

-- incoherent
-- instance (UnitalCombinatorialFreeAlgebra r a, CounitalCombinatorialFreeCoalgebra r c) => CounitalCombinatorialFreeCoalgebra (a -> r) c where counitCF k a = counitCF (`k` a)
-- instance CounitalCombinatorialFreeCoalgebra () a where counitCF _ = ()

instance Semiring r => CounitalCombinatorialFreeCoalgebra r () where
  counitCF f = f ()

instance (CounitalCombinatorialFreeCoalgebra r a, CounitalCombinatorialFreeCoalgebra r b) => CounitalCombinatorialFreeCoalgebra r (a, b) where
  counitCF k = counitCF $ \a -> counitCF $ \b -> k (a,b)

instance (CounitalCombinatorialFreeCoalgebra r a, CounitalCombinatorialFreeCoalgebra r b, CounitalCombinatorialFreeCoalgebra r c) => CounitalCombinatorialFreeCoalgebra r (a, b, c) where
  counitCF k = counitCF $ \a -> counitCF $ \b -> counitCF $ \c -> k (a,b,c)

instance (CounitalCombinatorialFreeCoalgebra r a, CounitalCombinatorialFreeCoalgebra r b, CounitalCombinatorialFreeCoalgebra r c, CounitalCombinatorialFreeCoalgebra r d) => CounitalCombinatorialFreeCoalgebra r (a, b, c, d) where
  counitCF k = counitCF $ \a -> counitCF $ \b -> counitCF $ \c -> counitCF $ \d -> k (a,b,c,d)

instance (CounitalCombinatorialFreeCoalgebra r a, CounitalCombinatorialFreeCoalgebra r b, CounitalCombinatorialFreeCoalgebra r c, CounitalCombinatorialFreeCoalgebra r d, CounitalCombinatorialFreeCoalgebra r e) => CounitalCombinatorialFreeCoalgebra r (a, b, c, d, e) where
  counitCF k = counitCF $ \a -> counitCF $ \b -> counitCF $ \c -> counitCF $ \d -> counitCF $ \e -> k (a,b,c,d,e)

instance Semiring r => CounitalCombinatorialFreeCoalgebra r [a] where
  counitCF k = k []

instance Semiring r => CounitalCombinatorialFreeCoalgebra r (Seq a) where
  counitCF k = k (Seq.empty)

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
