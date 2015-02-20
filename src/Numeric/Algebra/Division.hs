{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Numeric.Algebra.Division
  ( Division(..)
  ) where

import Prelude hiding ((*), recip, (/),(^))
import Numeric.Algebra.Class
import Numeric.Algebra.Unital

infixr 8 ^
infixl 7 /, \\

-- A multiplicative group
class Unital r => Division r where
  recip  :: r -> r
  (/)    :: r -> r -> r
  (\\)   :: r -> r -> r
  (^)    :: Integral n => r -> n -> r
  recip a = one / a
  a / b = a * recip b
  a \\ b = recip a * b
  x0 ^ y0 = case compare y0 0 of
    LT -> f (recip x0) (negate y0)
    EQ -> one
    GT -> f x0 y0
    where
       f x y 
         | even y = f (x * x) (y `quot` 2)
         | y == 1 = x
         | otherwise = g (x * x) ((y - 1) `quot` 2) x
       g x y z 
         | even y = g (x * x) (y `quot` 2) z
         | y == 1 = x * z
         | otherwise = g (x * x) ((y - 1) `quot` 2) (x * z)

instance Division () where 
  _ / _   = ()
  recip _ = ()
  _ \\ _  = ()
  _ ^ _   = ()

instance (Division a, Division b) => Division (a,b) where
  recip (a,b) = (recip a, recip b)
  (a,b) / (i,j) = (a/i,b/j)
  (a,b) \\ (i,j) = (a\\i,b\\j)
  (a,b) ^ n = (a^n,b^n)

instance (Division a, Division b, Division c) => Division (a,b,c) where
  recip (a,b,c) = (recip a, recip b, recip c)
  (a,b,c) / (i,j,k) = (a/i,b/j,c/k)
  (a,b,c) \\ (i,j,k) = (a\\i,b\\j,c\\k)
  (a,b,c) ^ n = (a^n,b^n,c^n)

instance (Division a, Division b, Division c, Division d) => Division (a,b,c,d) where
  recip (a,b,c,d) = (recip a, recip b, recip c, recip d)
  (a,b,c,d) / (i,j,k,l) = (a/i,b/j,c/k,d/l)
  (a,b,c,d) \\ (i,j,k,l) = (a\\i,b\\j,c\\k,d\\l)
  (a,b,c,d) ^ n = (a^n,b^n,c^n,d^n)

instance (Division a, Division b, Division c, Division d, Division e) => Division (a,b,c,d,e) where
  recip (a,b,c,d,e) = (recip a, recip b, recip c, recip d, recip e)
  (a,b,c,d,e) / (i,j,k,l,m) = (a/i,b/j,c/k,d/l,e/m)
  (a,b,c,d,e) \\ (i,j,k,l,m) = (a\\i,b\\j,c\\k,d\\l,e\\m)
  (a,b,c,d,e) ^ n = (a^n,b^n,c^n,d^n,e^n)
