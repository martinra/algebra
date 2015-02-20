{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Algebra.Unital
  ( 
  -- * Unital Multiplication (Multiplicative monoid)
    Unital(..)
  , product
  -- * Unital Algebras
  , UnitalLeftAlgebra(..)
  , UnitalRightAlgebra(..)
  , UnitalAlgebra(..)
  ) where

import Numeric.Algebra.Class
import Numeric.Natural
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


class LeftAlgebra r a => UnitalLeftAlgebra r a where
  unitL :: r -> a

class RightAlgebra r a => UnitalRightAlgebra r a where
  unitR :: r -> a

class Algebra r a => UnitalAlgebra r a where
  unit :: r -> a

-- * This is undecidable, but in the combinatorial setting it is not
-- instance (Unital r, UnitalAlgebra r a) => Unital a where
--  one = unit one
