{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, TypeOperators #-}
module Numeric.CombinatorialFreeAlgebra.Involutive
  ( 
  -- * Involutive CombinatorialFreeAlgebras
  , InvolutiveCombinatorialFreeAlgebra(..)
  , InvolutiveCombinatorialFreeCoalgebra(..)
  , InvolutiveCombinatorialFreeBialgebra
  -- * Trivial Involution
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

-- inv is an associative algebra homomorphism
class (InvolutiveSemiring r, CombinatorialFreeAlgebra r a) => InvolutiveCombinatorialFreeAlgebra r a where
  inv :: (a -> r) -> a -> r

instance InvolutiveCombinatorialFreeAlgebra r h => InvolutiveMultiplication (h -> r) where
  adjoint = inv

instance ( TriviallyInvolutive r
         , TriviallyInvolutiveCombinatorialFreeAlgebra r a
         ) => TriviallyInvolutive (a -> r)

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
