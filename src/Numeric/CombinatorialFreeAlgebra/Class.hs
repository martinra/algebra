{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances, TypeOperators #-}
module Numeric.CombinatorialFreeAlgebra.Class 
  ( -- * Associative algebras
    CombinatorialFreeAlgebra(..)
  -- * Coassociative coalgebras
  , CombinatorialFreeCoalgebra(..)
  ) where

import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
-- import Data.Semigroup.Foldable
import Data.Sequence hiding (reverse,index)
import Data.Set (Set)
import Numeric.Algebra.Class
import Numeric.Additive.Class
import Prelude hiding ((*), (+), negate, subtract,(-), recip, (/), foldr, sum, product, replicate, concat)
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

-- | An associative algebra built with a free module over a semiring
class Semiring r => CombinatorialFreeAlgebra r a where
  mult :: (a -> a -> r) -> a -> r

instance CombinatorialFreeAlgebra r a => Multiplicative (a -> r) where
  f * g = mult $ \a b -> f a * g b

instance CombinatorialFreeAlgebra r a => Semiring (a -> r) 

instance CombinatorialFreeAlgebra () a where
  mult _ _ = ()

-- | The tensor algebra
instance Semiring r => CombinatorialFreeAlgebra r [a] where
  mult f = go [] where
    go ls rrs@(r:rs) = f (reverse ls) rrs + go (r:ls) rs
    go ls [] = f (reverse ls) []

-- | The tensor algebra
instance Semiring r => CombinatorialFreeAlgebra r (Seq a) where
  mult f = go Seq.empty where
    go ls s = case viewl s of
       EmptyL -> f ls s 
       r :< rs -> f ls s + go (ls |> r) rs

instance Semiring r => CombinatorialFreeAlgebra r () where
  mult f = f ()

instance (Semiring r, Ord a) => CombinatorialFreeAlgebra r (Set a) where
  mult f = go Set.empty where
    go ls s = case Set.minView s of
       Nothing -> f ls s
       Just (r, rs) -> f ls s + go (Set.insert r ls) rs
instance Semiring r => CombinatorialFreeAlgebra r IntSet where
  mult f = go IntSet.empty where
    go ls s = case IntSet.minView s of
       Nothing -> f ls s
       Just (r, rs) -> f ls s + go (IntSet.insert r ls) rs

instance (Semiring r, Monoidal r, Ord a, Partitionable b) => CombinatorialFreeAlgebra r (Map a b) -- where
--  mult f xs = case minViewWithKey xs of
--    Nothing -> zero 
--    Just ((k, r), rs) -> ...
instance (Semiring r, Monoidal r, Partitionable a) => CombinatorialFreeAlgebra r (IntMap a)

instance (CombinatorialFreeAlgebra r a, CombinatorialFreeAlgebra r b) => CombinatorialFreeAlgebra r (a,b) where
  mult f (a,b) = mult (\a1 a2 -> mult (\b1 b2 -> f (a1,b1) (a2,b2)) b) a

instance (CombinatorialFreeAlgebra r a, CombinatorialFreeAlgebra r b, CombinatorialFreeAlgebra r c) => CombinatorialFreeAlgebra r (a,b,c) where
  mult f (a,b,c) = mult (\a1 a2 -> mult (\b1 b2 -> mult (\c1 c2 -> f (a1,b1,c1) (a2,b2,c2)) c) b) a

instance (CombinatorialFreeAlgebra r a, CombinatorialFreeAlgebra r b, CombinatorialFreeAlgebra r c, CombinatorialFreeAlgebra r d) => CombinatorialFreeAlgebra r (a,b,c,d) where
  mult f (a,b,c,d) = mult (\a1 a2 -> mult (\b1 b2 -> mult (\c1 c2 -> mult (\d1 d2 -> f (a1,b1,c1,d1) (a2,b2,c2,d2)) d) c) b) a

instance (CombinatorialFreeAlgebra r a, CombinatorialFreeAlgebra r b, CombinatorialFreeAlgebra r c, CombinatorialFreeAlgebra r d, CombinatorialFreeAlgebra r e) => CombinatorialFreeAlgebra r (a,b,c,d,e) where
  mult f (a,b,c,d,e) = mult (\a1 a2 -> mult (\b1 b2 -> mult (\c1 c2 -> mult (\d1 d2 -> mult (\e1 e2 -> f (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2)) e) d) c) b) a

-- incoherent
-- instance (CombinatorialFreeAlgebra r b, CombinatorialFreeAlgebra r a) => CombinatorialFreeAlgebra (b -> r) a where mult f a b = mult (\a1 a2 -> f a1 a2 b) a

-- A coassociative coalgebra over a semiring using
class Semiring r => CombinatorialFreeCoalgebra r c where
  comult :: (c -> r) -> c -> c -> r

-- | Every coalgebra gives rise to an algebra by vector space duality classically.
-- Sadly, it requires vector space duality, which we cannot use constructively.
-- The dual argument only relies in the fact that any constructive coalgebra can only inspect a finite number of coefficients, 
-- which we CAN exploit.
instance CombinatorialFreeAlgebra r m => CombinatorialFreeCoalgebra r (m -> r) where
  comult k f g = k (f * g)

-- instance CombinatorialFreeCoalgebra () c where comult _ _ _ = ()
-- instance (CombinatorialFreeAlgebra r b, CombinatorialFreeCoalgebra r c) => CombinatorialFreeCoalgebra (b -> r) c where comult f c1 c2 b = comult (`f` b) c1 c2 

instance Semiring r => CombinatorialFreeCoalgebra r () where
  comult = const

instance (CombinatorialFreeCoalgebra r a, CombinatorialFreeCoalgebra r b) => CombinatorialFreeCoalgebra r (a, b) where
  comult f (a1,b1) (a2,b2) = comult (\a -> comult (\b -> f (a,b)) b1 b2) a1 a2

instance (CombinatorialFreeCoalgebra r a, CombinatorialFreeCoalgebra r b, CombinatorialFreeCoalgebra r c) => CombinatorialFreeCoalgebra r (a, b, c) where
  comult f (a1,b1,c1) (a2,b2,c2) = comult (\a -> comult (\b -> comult (\c -> f (a,b,c)) c1 c2) b1 b2) a1 a2

instance (CombinatorialFreeCoalgebra r a, CombinatorialFreeCoalgebra r b, CombinatorialFreeCoalgebra r c, CombinatorialFreeCoalgebra r d) => CombinatorialFreeCoalgebra r (a, b, c, d) where
  comult f (a1,b1,c1,d1) (a2,b2,c2,d2) = comult (\a -> comult (\b -> comult (\c -> comult (\d -> f (a,b,c,d)) d1 d2) c1 c2) b1 b2) a1 a2

instance (CombinatorialFreeCoalgebra r a, CombinatorialFreeCoalgebra r b, CombinatorialFreeCoalgebra r c, CombinatorialFreeCoalgebra r d, CombinatorialFreeCoalgebra r e) => CombinatorialFreeCoalgebra r (a, b, c, d, e) where
  comult f (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = comult (\a -> comult (\b -> comult (\c -> comult (\d -> comult (\e -> f (a,b,c,d,e)) e1 e2) d1 d2) c1 c2) b1 b2) a1 a2

-- | The tensor Hopf algebra
instance Semiring r => CombinatorialFreeCoalgebra r [a] where
  comult f as bs = f (mappend as bs)

-- | The tensor Hopf algebra
instance Semiring r => CombinatorialFreeCoalgebra r (Seq a) where
  comult f as bs = f (mappend as bs)

-- | the free commutative band coalgebra
instance (Semiring r, Ord a) => CombinatorialFreeCoalgebra r (Set a) where
  comult f as bs = f (Set.union as bs)

-- | the free commutative band coalgebra over Int
instance Semiring r => CombinatorialFreeCoalgebra r IntSet where
  comult f as bs = f (IntSet.union as bs)

-- | the free commutative coalgebra over a set and a given semigroup
instance (Semiring r, Ord a, Additive b) => CombinatorialFreeCoalgebra r (Map a b) where
  comult f as bs = f (Map.unionWith (+) as bs)

-- | the free commutative coalgebra over a set and Int
instance (Semiring r, Additive b) => CombinatorialFreeCoalgebra r (IntMap b) where
  comult f as bs = f (IntMap.unionWith (+) as bs)

