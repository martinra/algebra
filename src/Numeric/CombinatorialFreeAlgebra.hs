module Numeric.CombinatorialFreeAlgebra
  ( -- ** associative algebras over (non-commutative) semirings 
    CombinatorialFreeAlgebra(..)
  , CombinatorialFreeCoalgebra(..)
  -- ** unital algebras
  , UnitalCombinatorialFreeAlgebra(..)
  , CounitalCombinatorialFreeCoalgebra(..)
  , CombinatorialFreeBialgebra
  -- ** involutive algebras
  , InvolutiveCombinatorialFreeAlgebra(..)
  , InvolutiveCombinatorialFreeCoalgebra(..)
  , InvolutiveCombinatorialFreeBialgebra
  , TriviallyInvolutiveCombinatorialFreeAlgebra
  , TriviallyInvolutiveCombinatorialFreeCoalgebra
  , TriviallyInvolutiveCombinatorialFreeBialgebra
  -- ** idempotent algebras
  , IdempotentCombinatorialFreeAlgebra
  , IdempotentCombinatorialFreeBialgebra
  -- ** commutative algebras
  , CommutativeCombinatorialFreeAlgebra
  , CommutativeCombinatorialFreeBialgebra
  , CocommutativeCombinatorialFreeCoalgebra
  -- ** division algebras
  , DivisionCombinatorialFreeAlgebra(..)
  -- ** Hopf alegebras
  , HopfCombinatorialFreeAlgebra(..)


  -- * Representable Multiplicative (via CombinatorialFreeAlgebra)
  , mulRep
  -- * Representable Unital (via UnitalCombinatorialFreeAlgebra)
  , oneRep
  -- * Representable Rig (via CombinatorialFreeAlgebra)
  , fromNaturalRep
  -- * Representable Ring (via CombinatorialFreeAlgebra)
  , fromIntegerRep
  ) where

import Prelude ()
import Numeric.CombinatorialFreeAlgebra.Class
import Numeric.CombinatorialFreeAlgebra.Involutive
import Numeric.CombinatorialFreeAlgebra.Idempotent
import Numeric.CombinatorialFreeAlgebra.Commutative
import Numeric.CombinatorialFreeAlgebra.Division
import Numeric.CombinatorialFreeAlgebra.Unital
import Numeric.CombinatorialFreeAlgebra.Hopf
import Numeric.Module.Representable
