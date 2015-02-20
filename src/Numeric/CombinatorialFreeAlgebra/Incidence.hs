{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , UndecidableInstances
           , DeriveDataTypeable
           #-}

module Numeric.CombinatorialFreeAlgebra.Incidence
  ( Interval(..)
  , zeta
  , moebius
  ) where

import Data.Data
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Algebra.Commutative
import Numeric.CombinatorialFreeAlgebra.Class
import Numeric.CombinatorialFreeAlgebra.Unital
import Numeric.Ring.Class
import Numeric.Order.Class
import Numeric.Order.LocallyFinite

-- the basis for an incidence algebra
data Interval a = Interval a a deriving (Eq,Ord,Show,Read,Data,Typeable)

instance (Commutative r, Monoidal r, Semiring r, LocallyFiniteOrder a) => CombinatorialFreeAlgebra r (Interval a) where
  mult f (Interval a c) = sumWith (\b -> f (Interval a b) (Interval b c)) $ range a c
  
instance (Commutative r, Monoidal r, Semiring r, LocallyFiniteOrder a) => UnitalCombinatorialFreeAlgebra r (Interval a) where
  unitCF r (Interval a b) 
    | a ~~ b = r
    | otherwise = zero

zeta :: Unital r => Interval a -> r
zeta = const one

moebius :: (Ring r, LocallyFiniteOrder a) => Interval a -> r
moebius (Interval a b) = moebiusInversion a b
