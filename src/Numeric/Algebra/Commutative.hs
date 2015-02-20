{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances, TypeOperators #-}
module Numeric.Algebra.Commutative 
  ( Commutative
  , CommutativeCombinatorialFreeAlgebra
  , CocommutativeCombinatorialFreeCoalgebra
  , CommutativeBialgebra
  ) where

import Data.Int
import Data.IntSet (IntSet)
import Data.IntMap (IntMap)
import Data.Set (Set)
import Data.Map (Map)
import Data.Word
import Numeric.Additive.Class
import Numeric.Algebra.Class
import Numeric.Algebra.Unital
import Numeric.Natural
import Prelude (Bool, Ord, Integer)



-- | A commutative multiplicative semigroup
class Multiplicative r => Commutative r

instance Commutative () 
instance Commutative Bool
instance Commutative Integer
instance Commutative Int
instance Commutative Int8
instance Commutative Int16
instance Commutative Int32
instance Commutative Int64
instance Commutative Natural
instance Commutative Word
instance Commutative Word8
instance Commutative Word16
instance Commutative Word32
instance Commutative Word64

instance ( Commutative a
         , Commutative b
         ) => Commutative (a,b) 

instance ( Commutative a
         , Commutative b
         , Commutative c
         ) => Commutative (a,b,c) 

instance ( Commutative a
         , Commutative b
         , Commutative c
         , Commutative d
         ) => Commutative (a,b,c,d) 

instance ( Commutative a
         , Commutative b
         , Commutative c
         , Commutative d
         , Commutative e
         ) => Commutative (a,b,c,d,e)

instance CommutativeCombinatorialFreeAlgebra r a => Commutative (a -> r)

class CombinatorialFreeAlgebra r a => CommutativeCombinatorialFreeAlgebra r a

instance ( Commutative r
         , Semiring r
         ) => CommutativeCombinatorialFreeAlgebra r ()

instance ( CommutativeCombinatorialFreeAlgebra r a
         , CommutativeCombinatorialFreeAlgebra r b
         ) => CommutativeCombinatorialFreeAlgebra r (a,b)

instance ( CommutativeCombinatorialFreeAlgebra r a
         , CommutativeCombinatorialFreeAlgebra r b
         , CommutativeCombinatorialFreeAlgebra r c
         ) => CommutativeCombinatorialFreeAlgebra r (a,b,c)

instance ( CommutativeCombinatorialFreeAlgebra r a
         , CommutativeCombinatorialFreeAlgebra r b
         , CommutativeCombinatorialFreeAlgebra r c
         , CommutativeCombinatorialFreeAlgebra r d
         ) => CommutativeCombinatorialFreeAlgebra r (a,b,c,d)

instance ( CommutativeCombinatorialFreeAlgebra r a
         , CommutativeCombinatorialFreeAlgebra r b
         , CommutativeCombinatorialFreeAlgebra r c
         , CommutativeCombinatorialFreeAlgebra r d
         , CommutativeCombinatorialFreeAlgebra r e
         ) => CommutativeCombinatorialFreeAlgebra r (a,b,c,d,e)

instance ( Commutative r
         , Semiring r
         , Ord a
         ) => CommutativeCombinatorialFreeAlgebra r (Set a)

instance (Commutative r
         , Semiring r
         ) => CommutativeCombinatorialFreeAlgebra r IntSet

instance (Commutative r
         , Monoidal r
         , Semiring r
         , Ord a
         , Abelian b
         , Partitionable b
         ) => CommutativeCombinatorialFreeAlgebra r (Map a b)

instance ( Commutative r
         , Monoidal r
         , Semiring r
         , Abelian b
         , Partitionable b
         ) => CommutativeCombinatorialFreeAlgebra r (IntMap b)



class CombinatorialFreeCoalgebra r c => CocommutativeCombinatorialFreeCoalgebra r c

instance CommutativeCombinatorialFreeAlgebra r m => CocommutativeCombinatorialFreeCoalgebra r (m -> r)

instance (Commutative r, Semiring r) => CocommutativeCombinatorialFreeCoalgebra r ()

instance ( CocommutativeCombinatorialFreeCoalgebra r a
         , CocommutativeCombinatorialFreeCoalgebra r b
         ) => CocommutativeCombinatorialFreeCoalgebra r (a,b)

instance ( CocommutativeCombinatorialFreeCoalgebra r a
         , CocommutativeCombinatorialFreeCoalgebra r b
         , CocommutativeCombinatorialFreeCoalgebra r c
         ) => CocommutativeCombinatorialFreeCoalgebra r (a,b,c)

instance ( CocommutativeCombinatorialFreeCoalgebra r a
         , CocommutativeCombinatorialFreeCoalgebra r b
         , CocommutativeCombinatorialFreeCoalgebra r c
         , CocommutativeCombinatorialFreeCoalgebra r d
         ) => CocommutativeCombinatorialFreeCoalgebra r (a,b,c,d)

instance ( CocommutativeCombinatorialFreeCoalgebra r a
         , CocommutativeCombinatorialFreeCoalgebra r b
         , CocommutativeCombinatorialFreeCoalgebra r c
         , CocommutativeCombinatorialFreeCoalgebra r d
         , CocommutativeCombinatorialFreeCoalgebra r e
         ) => CocommutativeCombinatorialFreeCoalgebra r (a,b,c,d,e)

instance ( Commutative r
         , Semiring r
         , Ord a) => CocommutativeCombinatorialFreeCoalgebra r (Set a)

instance ( Commutative r
         , Semiring r
         ) => CocommutativeCombinatorialFreeCoalgebra r IntSet

instance ( Commutative r
         , Semiring r
         , Ord a
         , Abelian b
         ) => CocommutativeCombinatorialFreeCoalgebra r (Map a b)

instance ( Commutative r
         , Semiring r
         , Abelian b
         ) => CocommutativeCombinatorialFreeCoalgebra r (IntMap b)



class ( Bialgebra r h
      , CommutativeCombinatorialFreeAlgebra r h
      , CocommutativeCombinatorialFreeCoalgebra r h
      ) => CommutativeBialgebra r h

instance ( Bialgebra r h
         , CommutativeCombinatorialFreeAlgebra r h
         , CocommutativeCombinatorialFreeCoalgebra r h
         ) => CommutativeBialgebra r h
