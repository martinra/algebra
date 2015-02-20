{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances, FlexibleInstances, TypeOperators #-}
module Numeric.Algebra.Commutative 
  ( Commutative
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
