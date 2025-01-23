
-- | The @Reducing@ and @ReducingExtension@ gates
--
-- These compute
--
-- > initial*alpha^n + sum_{i=0}^{n-1} c[i]*alpha^(n-i)
--

{-# LANGUAGE StrictData, RecordWildCards #-}
module Gate.Custom.Reducing where

--------------------------------------------------------------------------------

import Data.Foldable
import Control.Monad

import Algebra.Goldilocks
import Algebra.GoldilocksExt
import Algebra.Expr

import Gate.Vars
import Gate.Computation

import Misc.Aux

--------------------------------------------------------------------------------

reducingGateConstraints :: Int -> Compute ()
reducingGateConstraints num_coeffs = do

  forM_ [0..num_coeffs-1] $ \i -> do
    commitExt $ prev i * alpha + fromBase (coeff i) - accum i

  where
    
    -- witness variables
    output       = wireExt 0
    alpha        = wireExt 2
    initial      = wireExt 4
    coeff i      = wire (6+i)
    accum i      = if i < num_coeffs - 1 then wireExt (6 + num_coeffs + 2*i) else output
    prev i       = if i == 0 then initial else accum (i-1)

--------------------------------------------------------------------------------

reducingExtensionGateConstraints :: Int -> Compute ()
reducingExtensionGateConstraints num_coeffs = do

  forM_ [0..num_coeffs-1] $ \i -> do
    commitExt $ prev i * alpha + coeff i - accum i

  where

    -- witness variables
    output       = wireExt 0
    alpha        = wireExt 2
    initial      = wireExt 4
    coeff i      = wireExt (6+2*i)
    accum i      = if i < num_coeffs - 1 then wireExt (6 + 2*num_coeffs + 2*i) else output
    prev i       = if i == 0 then initial else accum (i-1)

--------------------------------------------------------------------------------
