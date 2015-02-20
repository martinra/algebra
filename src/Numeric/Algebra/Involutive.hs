{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, TypeOperators #-}
module Numeric.Algebra.Involutive
  ( 
  -- * Involution
    InvolutiveMultiplication(..)
  , InvolutiveSemiring
  -- * Trivial Involution
  , TriviallyInvolutive
  ) where

import Data.Int
import Data.Word
import Numeric.Algebra.Class
import Numeric.Algebra.Commutative
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
