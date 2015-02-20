module Numeric.Algebra
  ( 
  -- * Additive

  -- ** additive semigroups
    Additive(..)
  , sum1
  -- ** additive Abelian semigroups
  , Abelian
  -- ** additive idempotent semigroups
  , Idempotent
  , sinnum1pIdempotent
  , sinnumIdempotent
  -- ** partitionable additive semigroups
  , Partitionable(..)
  -- ** additive monoids
  , Monoidal(..)
  , sum
  -- ** additive groups
  , Group(..)

  -- * Multiplicative
  
  -- ** multiplicative semigroups
  , Multiplicative(..)
  , product1
  -- ** commutative multiplicative semigroups
  , Commutative
  -- ** multiplicative monoids
  , Unital(..)
  , product
  -- ** idempotent multiplicative semigroups
  , Band
  , pow1pBand
  , powBand
  -- ** multiplicative groups
  , Division(..)
  -- ** factorable multiplicative semigroups
  , Factorable(..)
  -- ** involutive multiplicative semigroups
  , InvolutiveMultiplication(..)
  , TriviallyInvolutive

  -- * Ring-Structures
  -- ** Semirings 
  , Semiring
  , InvolutiveSemiring
  , Dioid
  -- ** Rngs
  , Rng
  -- ** Rigs
  , Rig(..)
  -- * Rings
  , Ring(..)
  -- ** Division Rings
  , LocalRing
  , DivisionRing
  , Field

  -- * Modules
  , LeftModule(..)
  , RightModule(..)
  , Module

  -- * Ring Properties
  -- ** Characteristic
  , Characteristic(..)
  , charInt, charWord
  -- ** Order
  , Order(..)
  , OrderedRig
  , AdditiveOrder
  , LocallyFiniteOrder

  , DecidableZero
  , DecidableUnits
  , DecidableAssociates

  -- * Natural numbers
  , Natural
-- * Representable Additive
  , addRep, sinnum1pRep
  -- * Representable Monoidal
  , zeroRep, sinnumRep
  -- * Representable Group
  , negateRep, minusRep, subtractRep, timesRep
  
  -- * Norm
  , Quadrance(..)

  -- * Covectors
  , Covector(..)
  -- ** Covectors as linear functionals
  , counitM
  , unitM
  , comultM
  , multM
  , invM
  , coinvM
  , antipodeM
  , convolveM
  ) where

import Prelude ()
import Numeric.Additive.Class
import Numeric.Additive.Group
import Numeric.Algebra.Class
import Numeric.Algebra.Involutive
import Numeric.Algebra.Idempotent
import Numeric.Algebra.Commutative
import Numeric.Algebra.Division
import Numeric.Algebra.Factorable
import Numeric.Algebra.Unital
import Numeric.Covector
import Numeric.Decidable.Units
import Numeric.Decidable.Associates
import Numeric.Decidable.Zero
import Numeric.Dioid.Class
import Numeric.Module.Representable
import Numeric.Natural
import Numeric.Order.Class
import Numeric.Order.Additive
import Numeric.Order.LocallyFinite
import Numeric.Quadrance.Class
import Numeric.Rig.Class
import Numeric.Rig.Characteristic
import Numeric.Rig.Ordered
import Numeric.Rng.Class
import Numeric.Ring.Class
import Numeric.Ring.Local
import Numeric.Ring.Division
import Numeric.Field.Class
