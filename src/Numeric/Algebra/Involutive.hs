{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, TypeOperators #-}
module Numeric.Algebra.Involutive
  ( 
  -- * Involution
    InvolutiveMultiplication(..)
  , InvolutiveSemiring
  -- * Involutive CombinatorialFreeAlgebras
  , InvolutiveCombinatorialFreeAlgebra(..)
  , InvolutiveCombinatorialFreeCoalgebra(..)
  , InvolutiveCombinatorialFreeBialgebra
  -- * Trivial Involution
  , TriviallyInvolutive
  , TriviallyInvolutiveCombinatorialFreeAlgebra
  , TriviallyInvolutiveCombinatorialFreeCoalgebra
  , TriviallyInvolutiveCombinatorialFreeBialgebra
  ) where

import Data.Int
import Data.Word
import Numeric.Algebra.Class
import Numeric.Algebra.Commutative
import Numeric.Algebra.Unital
import Numeric.Natural

-- | An semigroup with involution
-- 
-- > adjoint a * adjoint b = adjoint (b * a)
class Multiplicative r => InvolutiveMultiplication r where
  adjoint :: r -> r

instance InvolutiveMultiplication Int where adjoint = id
instance InvolutiveMultiplication Integer where adjoint = id
instance InvolutiveMultiplication Int8 where adjoint = id
instance InvolutiveMultiplication Int16 where adjoint = id
instance InvolutiveMultiplication Int32 where adjoint = id
instance InvolutiveMultiplication Int64 where adjoint = id
instance InvolutiveMultiplication Bool where adjoint = id
instance InvolutiveMultiplication Word where adjoint = id
instance InvolutiveMultiplication Natural where adjoint = id
instance InvolutiveMultiplication Word8 where adjoint = id
instance InvolutiveMultiplication Word16 where adjoint = id
instance InvolutiveMultiplication Word32 where adjoint = id
instance InvolutiveMultiplication Word64 where adjoint = id
instance InvolutiveMultiplication () where adjoint = id

instance 
  ( InvolutiveMultiplication a
  , InvolutiveMultiplication b
  ) => InvolutiveMultiplication (a,b) where
  adjoint (a,b) = (adjoint a, adjoint b)

instance 
  ( InvolutiveMultiplication a
  , InvolutiveMultiplication b
  , InvolutiveMultiplication c
  ) => InvolutiveMultiplication (a,b,c) where
  adjoint (a,b,c) = (adjoint a, adjoint b, adjoint c)

instance 
  ( InvolutiveMultiplication a
  , InvolutiveMultiplication b
  , InvolutiveMultiplication c
  , InvolutiveMultiplication d
  ) => InvolutiveMultiplication (a,b,c,d) where
  adjoint (a,b,c,d) = (adjoint a, adjoint b, adjoint c, adjoint d)

instance 
  ( InvolutiveMultiplication a
  , InvolutiveMultiplication b
  , InvolutiveMultiplication c
  , InvolutiveMultiplication d
  , InvolutiveMultiplication e
  ) => InvolutiveMultiplication (a,b,c,d,e) where
  adjoint (a,b,c,d,e) = (adjoint a, adjoint b, adjoint c, adjoint d, adjoint e)

instance InvolutiveCombinatorialFreeAlgebra r h => InvolutiveMultiplication (h -> r) where
  adjoint = inv

-- | adjoint (x + y) = adjoint x + adjoint y
class (Semiring r, InvolutiveMultiplication r) => InvolutiveSemiring r

instance InvolutiveSemiring ()
instance InvolutiveSemiring Bool
instance InvolutiveSemiring Integer
instance InvolutiveSemiring Int
instance InvolutiveSemiring Int8
instance InvolutiveSemiring Int16
instance InvolutiveSemiring Int32
instance InvolutiveSemiring Int64
instance InvolutiveSemiring Natural
instance InvolutiveSemiring Word
instance InvolutiveSemiring Word8
instance InvolutiveSemiring Word16
instance InvolutiveSemiring Word32
instance InvolutiveSemiring Word64

instance ( InvolutiveSemiring a
         , InvolutiveSemiring b
         ) => InvolutiveSemiring (a, b)

instance ( InvolutiveSemiring a
         , InvolutiveSemiring b
         , InvolutiveSemiring c
         ) => InvolutiveSemiring (a, b, c)

instance ( InvolutiveSemiring a
         , InvolutiveSemiring b
         , InvolutiveSemiring c
         , InvolutiveSemiring d
         ) => InvolutiveSemiring (a, b, c, d)

instance ( InvolutiveSemiring a
         , InvolutiveSemiring b
         , InvolutiveSemiring c
         , InvolutiveSemiring d
         , InvolutiveSemiring e
         ) => InvolutiveSemiring (a, b, c, d, e)


-- | 
-- > adjoint = id
class ( Commutative r
      , InvolutiveMultiplication r
      ) => TriviallyInvolutive r

instance TriviallyInvolutive Bool
instance TriviallyInvolutive Int
instance TriviallyInvolutive Integer
instance TriviallyInvolutive Int8
instance TriviallyInvolutive Int16
instance TriviallyInvolutive Int32
instance TriviallyInvolutive Int64
instance TriviallyInvolutive Word
instance TriviallyInvolutive Natural
instance TriviallyInvolutive Word8
instance TriviallyInvolutive Word16
instance TriviallyInvolutive Word32
instance TriviallyInvolutive Word64
instance TriviallyInvolutive ()

instance ( TriviallyInvolutive a
         , TriviallyInvolutive b
         ) => TriviallyInvolutive (a,b)

instance ( TriviallyInvolutive a
         , TriviallyInvolutive b
         , TriviallyInvolutive c
         ) => TriviallyInvolutive (a,b,c)

instance ( TriviallyInvolutive a
         , TriviallyInvolutive b
         , TriviallyInvolutive c
         , TriviallyInvolutive d
         ) => TriviallyInvolutive (a,b,c,d)

instance ( TriviallyInvolutive a
         , TriviallyInvolutive b
         , TriviallyInvolutive c
         , TriviallyInvolutive d
         , TriviallyInvolutive e
         ) => TriviallyInvolutive (a,b,c,d,e)

instance ( TriviallyInvolutive r
         , TriviallyInvolutiveCombinatorialFreeAlgebra r a
         ) => TriviallyInvolutive (a -> r)

-- inv is an associative algebra homomorphism
class (InvolutiveSemiring r, CombinatorialFreeAlgebra r a) => InvolutiveCombinatorialFreeAlgebra r a where
  inv :: (a -> r) -> a -> r

instance InvolutiveSemiring r => InvolutiveCombinatorialFreeAlgebra r () where
  inv = (adjoint .)

instance 
  ( InvolutiveCombinatorialFreeAlgebra r a
  , InvolutiveCombinatorialFreeAlgebra r b
  ) => InvolutiveCombinatorialFreeAlgebra r (a, b) where
  inv f (a,b) = 
    inv (\a' -> 
    inv (\b' -> f (a',b')) b) a

instance 
  ( InvolutiveCombinatorialFreeAlgebra r a
  , InvolutiveCombinatorialFreeAlgebra r b
  , InvolutiveCombinatorialFreeAlgebra r c
  ) => InvolutiveCombinatorialFreeAlgebra r (a, b, c) where
  inv f (a,b,c) =
    inv (\a' -> 
    inv (\b' ->
    inv (\c' -> f (a',b',c')) c) b) a

instance 
  ( InvolutiveCombinatorialFreeAlgebra r a
  , InvolutiveCombinatorialFreeAlgebra r b
  , InvolutiveCombinatorialFreeAlgebra r c
  , InvolutiveCombinatorialFreeAlgebra r d
  ) => InvolutiveCombinatorialFreeAlgebra r (a, b, c, d) where
  inv f (a,b,c,d) = 
    inv (\a' ->
    inv (\b' ->
    inv (\c' -> 
    inv (\d' -> f (a',b',c',d')) d) c) b) a

instance 
  ( InvolutiveCombinatorialFreeAlgebra r a
  , InvolutiveCombinatorialFreeAlgebra r b
  , InvolutiveCombinatorialFreeAlgebra r c
  , InvolutiveCombinatorialFreeAlgebra r d
  , InvolutiveCombinatorialFreeAlgebra r e
  ) => InvolutiveCombinatorialFreeAlgebra r (a, b, c, d, e) where
  inv f (a,b,c,d,e) = 
    inv (\a' -> 
    inv (\b' -> 
    inv (\c' -> 
    inv (\d' -> 
    inv (\e' -> f (a',b',c',d',e')) e) d) c) b) a



class ( CommutativeCombinatorialFreeAlgebra r a
      , TriviallyInvolutive r
      , InvolutiveCombinatorialFreeAlgebra r a
      ) => TriviallyInvolutiveCombinatorialFreeAlgebra r a

instance ( TriviallyInvolutive r
         , InvolutiveSemiring r
         ) => TriviallyInvolutiveCombinatorialFreeAlgebra r ()

instance ( TriviallyInvolutiveCombinatorialFreeAlgebra r a
         , TriviallyInvolutiveCombinatorialFreeAlgebra r b
         ) => TriviallyInvolutiveCombinatorialFreeAlgebra r (a, b) where

instance (TriviallyInvolutiveCombinatorialFreeAlgebra r a
         , TriviallyInvolutiveCombinatorialFreeAlgebra r b
         , TriviallyInvolutiveCombinatorialFreeAlgebra r c
         ) => TriviallyInvolutiveCombinatorialFreeAlgebra r (a, b, c) where

instance ( TriviallyInvolutiveCombinatorialFreeAlgebra r a
         , TriviallyInvolutiveCombinatorialFreeAlgebra r b
         , TriviallyInvolutiveCombinatorialFreeAlgebra r c
         , TriviallyInvolutiveCombinatorialFreeAlgebra r d
         ) => TriviallyInvolutiveCombinatorialFreeAlgebra r (a, b, c, d)

instance ( TriviallyInvolutiveCombinatorialFreeAlgebra r a
         , TriviallyInvolutiveCombinatorialFreeAlgebra r b
         , TriviallyInvolutiveCombinatorialFreeAlgebra r c
         , TriviallyInvolutiveCombinatorialFreeAlgebra r d
         , TriviallyInvolutiveCombinatorialFreeAlgebra r e
         ) => TriviallyInvolutiveCombinatorialFreeAlgebra r (a, b, c, d, e)



class ( InvolutiveSemiring r
      , CombinatorialFreeCoalgebra r c
      ) => InvolutiveCombinatorialFreeCoalgebra r c where
  coinv :: (c -> r) -> c -> r

instance InvolutiveSemiring r => InvolutiveCombinatorialFreeCoalgebra r () where
  coinv f c = adjoint (f c)

instance 
  ( InvolutiveCombinatorialFreeCoalgebra r a
  , InvolutiveCombinatorialFreeCoalgebra r b
  ) => InvolutiveCombinatorialFreeCoalgebra r (a, b) where
  coinv f (a,b) = 
    coinv (\a' -> 
    coinv (\b' -> f (a',b')) b) a

instance 
  ( InvolutiveCombinatorialFreeCoalgebra r a
  , InvolutiveCombinatorialFreeCoalgebra r b
  , InvolutiveCombinatorialFreeCoalgebra r c
  ) => InvolutiveCombinatorialFreeCoalgebra r (a, b, c) where
  coinv f (a,b,c) = 
    coinv (\a' -> 
    coinv (\b' -> 
    coinv (\c' -> f (a',b',c')) c) b) a

instance 
  ( InvolutiveCombinatorialFreeCoalgebra r a
  , InvolutiveCombinatorialFreeCoalgebra r b
  , InvolutiveCombinatorialFreeCoalgebra r c
  , InvolutiveCombinatorialFreeCoalgebra r d
  ) => InvolutiveCombinatorialFreeCoalgebra r (a, b, c, d) where
  coinv f (a,b,c,d) = 
    coinv (\a' -> 
    coinv (\b' -> 
    coinv (\c' -> 
    coinv (\d' -> f (a',b',c',d')) d) c) b) a

instance 
  ( InvolutiveCombinatorialFreeCoalgebra r a
  , InvolutiveCombinatorialFreeCoalgebra r b
  , InvolutiveCombinatorialFreeCoalgebra r c
  , InvolutiveCombinatorialFreeCoalgebra r d
  , InvolutiveCombinatorialFreeCoalgebra r e
  ) => InvolutiveCombinatorialFreeCoalgebra r (a, b, c, d, e) where
  coinv f (a,b,c,d,e) = 
    coinv (\a' -> 
    coinv (\b' -> 
    coinv (\c' -> 
    coinv (\d' -> 
    coinv (\e' -> f (a',b',c',d',e')) e) d) c) b) a



class ( CocommutativeCombinatorialFreeCoalgebra r a
      , TriviallyInvolutive r
      , InvolutiveCombinatorialFreeCoalgebra r a
      ) => TriviallyInvolutiveCombinatorialFreeCoalgebra r a

instance ( TriviallyInvolutive r
         , InvolutiveSemiring r
         ) => TriviallyInvolutiveCombinatorialFreeCoalgebra r ()

instance ( TriviallyInvolutiveCombinatorialFreeCoalgebra r a
         , TriviallyInvolutiveCombinatorialFreeCoalgebra r b
         ) => TriviallyInvolutiveCombinatorialFreeCoalgebra r (a, b)

instance ( TriviallyInvolutiveCombinatorialFreeCoalgebra r a
         , TriviallyInvolutiveCombinatorialFreeCoalgebra r b
         , TriviallyInvolutiveCombinatorialFreeCoalgebra r c
         ) => TriviallyInvolutiveCombinatorialFreeCoalgebra r (a, b, c)

instance ( TriviallyInvolutiveCombinatorialFreeCoalgebra r a
         , TriviallyInvolutiveCombinatorialFreeCoalgebra r b
         , TriviallyInvolutiveCombinatorialFreeCoalgebra r c
         , TriviallyInvolutiveCombinatorialFreeCoalgebra r d
         ) => TriviallyInvolutiveCombinatorialFreeCoalgebra r (a, b, c, d)

instance ( TriviallyInvolutiveCombinatorialFreeCoalgebra r a
         , TriviallyInvolutiveCombinatorialFreeCoalgebra r b
         , TriviallyInvolutiveCombinatorialFreeCoalgebra r c
         , TriviallyInvolutiveCombinatorialFreeCoalgebra r d
         , TriviallyInvolutiveCombinatorialFreeCoalgebra r e
         ) => TriviallyInvolutiveCombinatorialFreeCoalgebra r (a, b, c, d, e)



class ( CombinatorialFreeBialgebra r h
      , InvolutiveCombinatorialFreeAlgebra r h
      , InvolutiveCombinatorialFreeCoalgebra r h
      ) => InvolutiveCombinatorialFreeBialgebra r h

instance ( CombinatorialFreeBialgebra r h
         , InvolutiveCombinatorialFreeAlgebra r h
         , InvolutiveCombinatorialFreeCoalgebra r h
         ) => InvolutiveCombinatorialFreeBialgebra r h



class ( InvolutiveCombinatorialFreeBialgebra r h
      , TriviallyInvolutiveCombinatorialFreeAlgebra r h
      , TriviallyInvolutiveCombinatorialFreeCoalgebra r h
      ) => TriviallyInvolutiveCombinatorialFreeBialgebra r h

instance ( InvolutiveCombinatorialFreeBialgebra r h
         , TriviallyInvolutiveCombinatorialFreeAlgebra r h
         , TriviallyInvolutiveCombinatorialFreeCoalgebra r h
         ) => TriviallyInvolutiveCombinatorialFreeBialgebra r h
