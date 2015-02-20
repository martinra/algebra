{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.CombinatorialFreeAlgebra.Hopf
  ( HopfCombinatorialFreeAlgebra(..)
  ) where

import Numeric.Algebra.Unital

-- | A HopfCombinatorialFreeAlgebra algebra on a semiring, where the module is free.
--
-- When @antipode . antipode = id@ and antipode is an antihomomorphism then we are an InvolutiveCombinatorialFreeBialgebra with @inv = antipode@ as well

class CombinatorialFreeBialgebra r h => HopfCombinatorialFreeAlgebra r h where
  -- > convolve id antipode = convolve antipode id = unit . counit
  antipode :: (h -> r) -> h -> r

-- incoherent
-- instance (UnitalCombinatorialFreeAlgebra r a, HopfCombinatorialFreeAlgebra r h) => HopfCombinatorialFreeAlgebra (a -> r) h where antipode f h a = antipode (`f` a) h
-- instance HopfCombinatorialFreeAlgebra () h where antipode = id

-- TODO: check this
-- instance InvolutiveSemiring r => HopfCombinatorialFreeAlgebra r () where antipode = adjoint

instance (HopfCombinatorialFreeAlgebra r a, HopfCombinatorialFreeAlgebra r b) => HopfCombinatorialFreeAlgebra r (a, b) where
  antipode f (a,b) = antipode (\a' -> antipode (\b' -> f (a',b')) b) a

instance (HopfCombinatorialFreeAlgebra r a, HopfCombinatorialFreeAlgebra r b, HopfCombinatorialFreeAlgebra r c) => HopfCombinatorialFreeAlgebra r (a, b, c) where
  antipode f (a,b,c) = antipode (\a' -> antipode (\b' -> antipode (\c' -> f (a',b',c')) c) b) a

instance (HopfCombinatorialFreeAlgebra r a, HopfCombinatorialFreeAlgebra r b, HopfCombinatorialFreeAlgebra r c, HopfCombinatorialFreeAlgebra r d) => HopfCombinatorialFreeAlgebra r (a, b, c, d) where
  antipode f (a,b,c,d) = antipode (\a' -> antipode (\b' -> antipode (\c' -> antipode (\d' -> f (a',b',c',d')) d) c) b) a

instance (HopfCombinatorialFreeAlgebra r a, HopfCombinatorialFreeAlgebra r b, HopfCombinatorialFreeAlgebra r c, HopfCombinatorialFreeAlgebra r d, HopfCombinatorialFreeAlgebra r e) => HopfCombinatorialFreeAlgebra r (a, b, c, d, e) where
  antipode f (a,b,c,d,e) = antipode (\a' -> antipode (\b' -> antipode (\c' -> antipode (\d' -> antipode (\e' -> f (a',b',c',d',e')) e) d) c) b) a
