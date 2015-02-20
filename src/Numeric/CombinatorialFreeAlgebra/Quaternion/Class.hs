module Numeric.CombinatorialFreeAlgebra.Quaternion.Class
  ( Hamiltonian(..)
  ) where

import Numeric.CombinatorialFreeAlgebra.Complex.Class
import Numeric.Covector

class Complicated t => Hamiltonian t where
  j :: t
  k :: t

instance Hamiltonian a => Hamiltonian (Covector r a) where
  j = return j
  k = return k
