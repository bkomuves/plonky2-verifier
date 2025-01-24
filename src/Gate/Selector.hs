
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

data SelectorConfig = MkSelectorConfig 
  { numGateSelectors   :: Int       -- ^ number of gate selectors (usually 2-3)
  , numLookupSelectors :: Int       -- ^ number of lookup selectors (`4 + #nluts`)
  , numGateConstants   :: Int       -- ^ number of gate constants (normally 2)
  , numSigmaColumns    :: Int       -- ^ number of sigma columns (normally 80)
  }
  deriving Show

getSelectorConfig :: CommonCircuitData -> SelectorConfig
getSelectorConfig (MkCommonCircuitData{..}) 
  | circuit_num_lookup_selectors /= expected_lookup_sels
      = error "getSelectorConfig: fatal: num_lookup_selectors /= (4 + #nluts)"
  | circuit_num_constants /= num_gate_selectors + circuit_num_lookup_selectors + config_num_constants 
      = error "getSelectorConfig: fatal: constant columns tally does not add up!"
  | otherwise =  MkSelectorConfig 
      { numGateSelectors   = num_gate_selectors
      , numLookupSelectors = circuit_num_lookup_selectors
      , numGateConstants   = config_num_constants
      , numSigmaColumns    = config_num_routed_wires
      }
  where
    MkCircuitConfig{..}  = circuit_config
    nluts                = length circuit_luts
    expected_lookup_sels = if nluts == 0 then 0 else (4 + nluts)
    num_gate_selectors   = length (selector_groups circuit_selectors_info)  
    
    -- NOTE:
    --   circuit_num_constants = total number of constant columns (selectors + lookup_selectors + constants)
    --   config_num_constants  = only the gate constants

--------------------------------------------------------------------------------

data ConstantColumns a = MkConstantColumns
  { gateSelectors   :: [a]
  , lookupSelectors :: [a]
  , gateConstants   :: [a]
  }
  deriving Show

splitConstantColumns :: SelectorConfig -> [a] -> ConstantColumns a
splitConstantColumns (MkSelectorConfig{..}) xs 
   | not (null rest3)                 = error "splitConstantColumns: fatal: numbers do not add up"
   | length konst /= numGateConstants = error "splitConstantColumns: fatal: not enough constant columns"
   | otherwise = MkConstantColumns
      { gateSelectors   = gate_sel
      , lookupSelectors = lkp_sel
      , gateConstants   = konst
      }
   where
     (gate_sel,rest1) = splitAt numGateSelectors   xs
     (lkp_sel ,rest2) = splitAt numLookupSelectors rest1
     (konst   ,rest3) = splitAt numGateConstants   rest2

--------------------------------------------------------------------------------

-- | Given an evaluation point @x@ and a gate index @k@, we compute
-- the evaluation of the corresponding selector polynomial
--
-- Note: In the actual protocol, we have @x = S_g(zeta)@ 
--
evalGateSelectorPoly :: SelectorsInfo -> FExt -> Int -> FExt
evalGateSelectorPoly (MkSelectorsInfo{..}) x k = value where
  group_idx = selector_indices !! k
  range     = selector_groups !! group_idx
  initial   = if length selector_groups > 1 then unused - x else 1
  value     = initial * product [ fromIntegral j - x | j <- enumerateRange range , j /= k ] 
  unused    = (2^32 - 1) :: FExt

-- | Given the evaluations of the selector column polynomials, we evaluate
-- all the gate selectors
evalGateSelectors :: SelectorsInfo -> [FExt] -> [FExt]
evalGateSelectors selInfo@(MkSelectorsInfo{..}) xs = values where
  values = [ evalGateSelectorPoly selInfo (xs!!grp) i | (i,grp) <- zip [0..] selector_indices ]

{-
-- | Number of /gate selector/ column (does not include the lookup selectors!)
numGateSelectorColumns :: SelectorsInfo -> Int
numGateSelectorColumns selInfo = length (selector_groups selInfo)
-}

--------------------------------------------------------------------------------
