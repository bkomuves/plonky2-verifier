
-- | Selector polynomials

{-# LANGUAGE StrictData, RecordWildCards #-}
module Gate.Selector where

--------------------------------------------------------------------------------

import Data.Array hiding (range)

import Algebra.Goldilocks
import Algebra.GoldilocksExt
import Algebra.Expr

import Gate.Base
import Gate.Vars

import Types
import Misc.Aux

--------------------------------------------------------------------------------

-- | Given an evaluation point @x@ and a gate index @k@, we compute
-- the evaluation of the corresponding selector polynomial
--
-- Note: In the actual protocol, we have @x = S_g(zeta)@ 
--
evalSelectorPoly :: SelectorsInfo -> FExt -> Int -> FExt
evalSelectorPoly (MkSelectorsInfo{..}) x k = value where
  group_idx = selector_indices !! k
  range     = selector_groups !! group_idx
  initial   = if length selector_groups > 1 then unused - x else 1
  value     = initial * product [ fromIntegral j - x | j <- enumerateRange range , j /= k ] 
  unused    = (2^32 - 1) :: FExt

-- | Given the evaluations of the selector column polynomials, we evaluate
-- all the gate selectors
evalSelectors :: SelectorsInfo -> [FExt] -> [FExt]
evalSelectors selInfo@(MkSelectorsInfo{..}) xs = values where
  values = [ evalSelectorPoly selInfo (xs!!grp) i | (i,grp) <- zip [0..] selector_indices ]

numSelectorColumns :: SelectorsInfo -> Int
numSelectorColumns selInfo = length (selector_groups selInfo)

--------------------------------------------------------------------------------
