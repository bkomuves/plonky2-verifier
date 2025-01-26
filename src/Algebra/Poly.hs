
-- | Polynomials

module Algebra.Poly where

--------------------------------------------------------------------------------

import Algebra.Goldilocks
import Algebra.GoldilocksExt

--------------------------------------------------------------------------------

-- | Evaluate the Lagrange polynomial @L_0(x)@ of a subgroup of size @N@
evalLagrange0 :: Int -> FExt -> FExt
evalLagrange0 nn zeta 
  | zeta == 1  = 1
  | otherwise  = (powExt_ zeta nn - 1) / (fromIntegral nn * (zeta - 1))

--------------------------------------------------------------------------------

-- | Evaluate the zero polynomial @Z_H(x)@ on a subgroup of size @N@
evalZeroPoly :: Int -> FExt -> FExt
evalZeroPoly nn zeta = powExt_ zeta nn - 1 

--------------------------------------------------------------------------------
